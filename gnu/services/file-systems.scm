;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu services file-systems)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:autoload   (gnu packages autotools) (autoconf automake libtool)
  #:autoload   (gnu packages file-systems) (zfs)
  #:autoload   (gnu packages linux) (util-linux)
  #:autoload   (gnu packages onc-rpc) (libtirpc)
  #:autoload   (gnu packages pkg-config) (pkg-config)
  #:export (zfs-configuration
            zfs-configuration-zfs
            zfs-configuration-kernel-has-zfs-module?
            zfs-service-type
            linux-with-zfs))


;;;
;;; ZFS
;;;

(define-configuration/no-serialization zfs-configuration
  (zfs
   (file-like zfs)
   "The package to provide ZFS command-line utilities and udev rules.")
  (auto-mount?
   (boolean #t)
   "Auto-mount ZFS datasets.")
  (volumes?
   (boolean #t)
   "Wait for ZFS volumes to show up.")
  (kernel-has-zfs-module?
   (boolean #f)
   "Whether the kernel has builtin ZFS modules.  This must only be enabled when
using a @code{linux-with-zfs} kernel."))

(define zfs-linux-loadable-module-service
  (match-record-lambda <zfs-configuration>
      (zfs kernel-has-zfs-module?)
    (if kernel-has-zfs-module?
        '()
        (list `(,zfs "module")))))

(define zfs-shepherd-service
  (match-record-lambda <zfs-configuration>
      (zfs volumes? auto-mount?)
    (let ((zfs-mount-shepherd-service
           (shepherd-service
             (documentation "Mount all available ZFS file systems.")
             (provision '(zfs-mount))
             (requirement '(zfs-import))
             (start
              #~(make-system-constructor
                 (string-join
                  (list #$(file-append zfs "/sbin/zfs") "mount" "-a" "-l"))))
             (stop
              #~(make-system-destructor
                 (string-join
                  (list #$(file-append zfs "/sbin/zfs") "unmount" "-a"))))))
          (zfs-volumes-shepherd-service
           (shepherd-service
             (documentation "Wait for ZFS volume links to appear in /dev.")
             (provision '(zfs-volumes))
             (requirement '(zfs-import))
             (start
              #~(make-system-constructor
                 (string-join
                  ;; TODO: Patch references within zfs package instead.
                  (list "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin"
                        #$(file-append zfs "/bin/zvol_wait")))))
             (stop #~(const #f)))))
      (append
       (if auto-mount? (list zfs-mount-shepherd-service)   '())
       (if volumes?    (list zfs-volumes-shepherd-service) '())
       (list (shepherd-service
               (documentation "Import ZFS storage pools.")
               (provision '(zfs-import))
               (requirement '(udev))
               (start
                #~(make-system-constructor
                   (string-join
                    (list #$(file-append zfs "/sbin/zpool") "import" "-a" "-N"))))
               (stop #~(const #f)))
             (shepherd-service
               (documentation "Take care of ZFS file systems.")
               (provision '(file-system-zfs))
               (requirement
                `(zfs-import
                  ,@(if auto-mount? '(zfs-mount)   '())
                  ,@(if volumes?    '(zfs-volumes) '())))
               (start #~(const #t))
               (stop #~(const #f))))))))

(define zfs-service-type
  (service-type
    (name 'zfs)
    (extensions
     (list (service-extension profile-service-type
                              (compose list zfs-configuration-zfs))
           (service-extension linux-loadable-module-service-type
                              zfs-linux-loadable-module-service)
           (service-extension udev-service-type
                              (compose list zfs-configuration-zfs))
           (service-extension shepherd-root-service-type
                              zfs-shepherd-service)
           (service-extension user-processes-service-type
                              (const '(file-system-zfs)))))
    (default-value (zfs-configuration))
    (description "")))

(define* (linux-with-zfs linux #:optional (zfs zfs))
  "Return a kernel package based on LINUX, with ZFS kernel modules built in."

  (cond
   ((not (package? linux))
    (warning (G_ "'~a': kernel '~a' is not a package, skipping modification~%")
             "linux-with-zfs"
             linux)
    linux)
   ((assq-ref (package-properties linux) 'with-zfs?)
    linux)
   (else
    (let ((zfs-directory
           (string-append (package-name zfs) "-" (package-version zfs))))
      (package
        (inherit linux)
        (name (string-append (package-name linux) "-with-zfs"))
        (arguments
         (substitute-keyword-arguments arguments
           ((#:substitutable? _ #t) #f)
           ((#:phases phases)
            #~(modify-phases #$phases
                (add-after 'unpack 'unpack-zfs
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((zfs-source #+(package-source zfs)))
                      (if (file-is-directory? zfs-source)
                          (copy-recursively zfs-source #$zfs-directory)
                          (invoke "tar" "xf" zfs-source)))
                    (with-directory-excursion #$zfs-directory
                      ;; Copied from zfs package.
                      (substitute* "module/os/linux/zfs/zfs_ctldir.c"
                        (("/usr/bin/env\", \"umount")
                         (string-append
                          (search-input-file inputs "/bin/umount") "\", \"-n"))
                        (("/usr/bin/env\", \"mount")
                         (string-append
                          (search-input-file inputs "/bin/mount") "\", \"-n"))))))
                ;; https://github.com/openzfs/zfs/issues/10450#issuecomment-643654436
                (add-after 'patch-source-shebangs 'add-zfs-to-kernel-tree
                  (lambda _
                    (invoke "make" "defconfig" "prepare")
                    (with-directory-excursion #$zfs-directory
                      (invoke "./autogen.sh")
                      (substitute* "configure"
                        (("/bin/sh") (which "sh")))
                      (invoke "./configure" "--enable-linux-builtin"
                              "--with-linux=.."
                              "--with-linux-obj=..")
                      (invoke "./copy-builtin" ".."))
                    (delete-file-recursively #$zfs-directory)))
                (add-after 'configure 'enable-zfs
                  (lambda _
                    (for-each
                     (lambda (config)
                       (invoke "./scripts/config" "--enable" config))
                     '("CONFIG_CRYPTO_DEFLATE"
                       "CONFIG_ZLIB_DEFLATE"
                       "CONFIG_KALLSYMS"
                       "CONFIG_EFI_PARTITION"
                       "CONFIG_ZFS"))))))))
        (native-inputs
         (modify-inputs native-inputs
           (prepend autoconf
                    automake
                    libtool
                    pkg-config)))
        (inputs
         (modify-inputs inputs
           (prepend libtirpc
                    util-linux
                    `(,util-linux "lib"))))
        (properties
         `((with-zfs? . #t)
           ,@(package-properties linux))))))))
