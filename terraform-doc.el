;;; terraform-doc.el --- Explore a GitHub repository on the fly -*- lexical-binding: t -*-

;; Copyright (C) 2019 Giap Tran <txgvnn@gmail.com>

;; Author: Giap Tran <txgvnn@gmail.com>
;; URL: https://github.com/TxGVNN/terraform-doc
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; M-x terraform-doc

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup terraform nil
  "Major mode of Terraform configuration file."
  :group 'languages
  :prefix "terraform-doc-")

(defcustom terraform-doc-hook nil
  "*Hook run by `terraform-doc'."
  :type 'hook
  :group 'terraform)

(defcustom terraform-doc-name "Terraform"
  "*Modeline of `terraform-doc'."
  :type 'string
  :group 'terraform)

(defvar terraform-doc-providers
  '("acme" "akamai" "alicloud" "archive" "arukas" "avi" "aws" "azurerm" "azuread" "azurestack" "bitbucket" "brightbox" "clc" "chef" "circonus" "ciscoasa" "aci" "cloudflare" "cloudscale" "cloudstack" "cobbler" "consul" "datadog" "do" "dns" "dnsimple" "dme" "docker" "dyn" "external" "bigip" "fastly" "flexibleengine" "fortios" "github" "gitlab" "google" "grafana" "gridscale" "hedvig" "helm" "heroku" "hcloud" "http" "huaweicloud" "icinga2" "ignition" "influxdb" "jdcloud" "kubernetes" "librato" "linode" "local" "logentries" "logicmonitor" "mailgun" "mysql" "ncloud" "netlify" "newrelic" "nomad" "ns1" "null" "nutanix" "oneandone" "openstack" "opentelekomcloud" "opsgenie" "oci" "oraclepaas" "opc" "ovh" "packet" "pagerduty" "panos" "postgresql" "powerdns" "profitbricks" "rabbitmq" "rancher" "rancher2" "random" "rightscale" "rundeck" "runscope" "scaleway" "selectel" "signalfx" "skytap" "softlayer" "spotinst" "statuscake" "telefonicaopencloud" "template" "tencentcloud" "terraform" "tfe" "tls" "triton" "ucloud" "ultradns" "vault" "nsxt" "vcd" "vra7" "vsphere" "yandex"))

;;;###autoload
(defun terraform-doc (&optional provider)
  "Lookup PROVIDER."
  (interactive (list (completing-read "Provider: " terraform-doc-providers)))
  (terraform-doc--render-object
   (format "https://www.terraform.io/docs/providers/%s/index.html" provider)
   (format "*Terraform/provider:%s*" provider)))

(defun terraform-doc--render-object (url buffer-name)
  "Render the URL and rename to BUFFER-NAME."
  (if (get-buffer buffer-name)
      (switch-to-buffer buffer-name)
    (url-retrieve
     url
     (lambda (arg)
       (cond
        ((equal :error (car arg))
         (message arg))
        (t
         (with-current-buffer (current-buffer)
           (goto-char (point-min))
           (search-forward "<div id=\"inner\"")
           (beginning-of-line)
           (delete-region (point) (point-min))
           (search-forward "<div id=\"footer\"")
           (search-backward "</div>" nil nil 2)
           (delete-region (point) (point-max))
           (shr-render-region (point-min) (point-max))
           (goto-char (point-min))
           (rename-buffer buffer-name)
           (terraform-doc-mode)
           (switch-to-buffer (current-buffer)))))))))


(define-derived-mode terraform-doc-mode special-mode terraform-doc-name
  "Major mode for exploring Terraform repository on the fly"
  (setq buffer-auto-save-file-name nil
        buffer-read-only t))

(provide 'terraform-doc)
;;; terraform-doc.el ends here
