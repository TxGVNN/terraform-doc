;;; terraform-doc.el --- Look up terraform documentation on the fly -*- lexical-binding: t -*-

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
(require 'shr)

(defgroup terraform nil
  "Major mode of terraform-doc file."
  :group 'languages
  :prefix "terraform-doc-")

(defcustom terraform-doc-hook nil
  "*Hook run by `terraform-doc'."
  :type 'hook
  :group 'terraform)

(defcustom terraform-doc-name "Terraform-Doc"
  "*Modeline of `terraform-doc'."
  :type 'string
  :group 'terraform)

(defvar terraform-doc-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "o") 'terraform-doc-at-point)
    (define-key keymap (kbd "RET") 'terraform-doc-at-point)
    (define-key keymap (kbd "<tab>") 'shr-next-link)
    (define-key keymap (kbd "TAB") 'shr-next-link)
    (define-key keymap (kbd "n") 'next-line)
    (define-key keymap (kbd "p") 'previous-line)
    keymap)
  "Keymap for Terraform-Doc major mode.")

(defvar terraform-doc-url-base "https://www.terraform.io")
(defvar terraform-doc-url-temp nil)

(defvar terraform-doc-providers
  '(("ACME" . "acme") ("Akamai" . "akamai") ("Alibaba Cloud" . "alicloud") ("Archive" . "archive") ("Arukas" . "arukas") ("Avi Vantage" . "avi") ("AWS" . "aws") ("Azure" . "azurerm") ("Azure Active Directory" . "azuread") ("Azure Stack" . "azurestack") ("Bitbucket" . "bitbucket") ("Brightbox" . "brightbox") ("CenturyLinkCloud" . "clc") ("Chef" . "chef") ("Circonus" . "circonus") ("Cisco ASA" . "ciscoasa") ("Cisco ACI" . "aci") ("Cloudflare" . "cloudflare") ("CloudScale.ch" . "cloudscale") ("CloudStack" . "cloudstack") ("Cobbler" . "cobbler") ("Consul" . "consul")
    ("Datadog" . "datadog") ("DigitalOcean" . "do") ("DNS" . "dns") ("DNSimple" . "dnsimple") ("DNSMadeEasy" . "dme") ("Docker" . "docker") ("Dyn" . "dyn") ("External" . "external") ("F5 BIG-IP" . "bigip") ("Fastly" . "fastly") ("FlexibleEngine" . "flexibleengine") ("FortiOS" . "fortios") ("GitHub" . "github") ("GitLab" . "gitlab") ("Google Cloud Platform" . "google") ("Grafana" . "grafana") ("Gridscale" . "gridscale") ("Hedvig" . "hedvig") ("Helm" . "helm") ("Heroku" . "heroku") ("Hetzner Cloud" . "hcloud") ("HTTP" . "http") ("HuaweiCloud" . "huaweicloud") ("Icinga2" . "icinga2")
    ("Ignition" . "ignition") ("InfluxDB" . "influxdb") ("JDCloud" . "jdcloud") ("Kubernetes" . "kubernetes") ("Librato" . "librato") ("Linode" . "linode") ("Local" . "local") ("Logentries" . "logentries") ("LogicMonitor" . "logicmonitor") ("Mailgun" . "mailgun") ("MySQL" . "mysql") ("Naver Cloud" . "ncloud") ("Netlify" . "netlify") ("New Relic" . "newrelic") ("Nomad" . "nomad") ("NS1" . "ns1") ("Null" . "null") ("Nutanix" . "nutanix") ("1 and 1" . "oneandone") ("OpenStack" . "openstack") ("OpenTelekomCloud" . "opentelekomcloud") ("OpsGenie" . "opsgenie")
    ("Oracle Cloud Infrastructure" . "oci") ("Oracle Cloud Platform" . "oraclepaas") ("Oracle Public Cloud" . "opc") ("OVH" . "ovh") ("Packet" . "packet") ("PagerDuty" . "pagerduty") ("Palo Alto Networks" . "panos") ("PostgreSQL" . "postgresql") ("PowerDNS" . "powerdns") ("ProfitBricks" . "profitbricks") ("RabbitMQ" . "rabbitmq") ("Rancher" . "rancher") ("Rancher2" . "rancher2") ("Random" . "random") ("RightScale" . "rightscale") ("Rundeck" . "rundeck") ("RunScope" . "runscope") ("Scaleway" . "scaleway") ("Selectel" . "selectel") ("SignalFx" . "signalfx") ("Skytap" . "skytap")
    ("SoftLayer" . "softlayer") ("Spotinst" . "spotinst") ("StatusCake" . "statuscake") ("TelefonicaOpenCloud" . "telefonicaopencloud") ("Template" . "template") ("TencentCloud" . "tencentcloud") ("Terraform" . "terraform") ("Terraform Enterprise" . "tfe") ("TLS" . "tls") ("Triton" . "triton") ("UCloud" . "ucloud") ("UltraDNS" . "ultradns") ("Vault" . "vault") ("VMware NSX-T" . "nsxt") ("VMware vCloud Director" . "vcd") ("VMware vRA7" . "vra7") ("VMware vSphere" . "vsphere") ("Yandex" . "yandex")))

;;;###autoload
(defun terraform-doc (&optional provider)
  "Look up PROVIDER."
  (interactive (list
                (cdr (assoc (completing-read
                             "Provider: "
                             (mapcar (lambda (x) (car x)) terraform-doc-providers))
                            terraform-doc-providers))))
  (terraform-doc--render-tree
   (format "%s/docs/providers/%s/index.html" terraform-doc-url-base provider)
   (format "*Terraform:providers/%s*" provider)))

(defun terraform-doc-at-point()
  "Render url by 'terraform-doc--render-object."
  (interactive)
  (if (get-text-property (point) 'shr-url)
      (let* ((url (get-text-property (point) 'shr-url))
             (buffer-name  (replace-regexp-in-string
                            (format "%s/docs/\\(.*\\).html" terraform-doc-url-base) "\\1" url)))
        (if (string-match-p (regexp-quote terraform-doc-url-base) url)
            (terraform-doc--render-object url (format "*Terraform:%s*" buffer-name))
          (eww url)))))

(defun terraform-doc--render-tree (url buffer-name)
  "Render the URL and rename to BUFFER-NAME."
  (if (get-buffer buffer-name)
      (switch-to-buffer buffer-name)
    (setq terraform-doc-url-temp url)
    (url-retrieve
     url
     (lambda (arg)
       (cond
        ((equal :error (car arg))
         (message arg))
        (t
         (terraform-doc--modify-source (current-buffer) 'tree)
         (rename-buffer buffer-name)
         (terraform-doc-mode-on)))))))

(defun terraform-doc--render-object (url buffer-name)
  "Render the URL and rename to BUFFER-NAME."
  (if (get-buffer buffer-name)
      (switch-to-buffer buffer-name)
    (setq terraform-doc-url-temp url)
    (url-retrieve
     url
     (lambda (arg)
       (cond
        ((equal :error (car arg))
         (message arg))
        (t
         (terraform-doc--modify-source (current-buffer) 'object)
         (rename-buffer buffer-name)
         (terraform-doc-mode-on)))))))

(defun terraform-doc--modify-source (buffer type)
  "Modify source code in BUFFER with TYPE."
  (with-current-buffer buffer
    (cond
     ((equal type 'tree)
      (goto-char (point-min))
      (search-forward "<ul class=\"nav docs-sidenav\">")
      (search-forward "</li>")
      (beginning-of-line)
      (delete-region (point) (point-min))
      (search-forward "</div>")
      (beginning-of-line)
      (delete-region (point) (point-max)))
     ((equal type 'object)
      (goto-char (point-min))
      (search-forward "<div id=\"inner\"")
      (beginning-of-line)
      (delete-region (point) (point-min))
      (search-forward "<div id=\"footer\"")
      (search-backward "</div>" nil nil 2)
      (delete-region (point) (point-max))))
    (goto-char (point-min))
    (while (re-search-forward "&raquo;" nil t)
      (replace-match "*" nil nil))
    (goto-char (point-min))
    (while (re-search-forward "href=\"#.*?\">" nil t)
      (replace-match (format "href=\"%s\">" terraform-doc-url-temp) nil nil))
    (goto-char (point-min))
    (while (re-search-forward "href=\"/" nil t)
      (replace-match (format "href=\"%s/" terraform-doc-url-base) nil nil))))

(defun terraform-doc-mode-on ()
  "Render and switch to ‘terraform-doc’ mode."
  (shr-render-region (point-min) (point-max))
  (goto-char (point-min))
  (terraform-doc-mode)
  (switch-to-buffer (current-buffer)))

(define-derived-mode terraform-doc-mode special-mode terraform-doc-name
  "Major mode for looking up terraform documentation on the fly."
  (local-set-key [remap shr-browse-url] 'terraform-doc-at-point)
  (setq buffer-auto-save-file-name nil
        buffer-read-only t))

(provide 'terraform-doc)
;;; terraform-doc.el ends here
