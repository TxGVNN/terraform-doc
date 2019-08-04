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
  '(("ACME" . "acme") ("Akamai" . "akamai") ("Alibaba Cloud" . "alicloud") ("Archive" . "archive") ("Arukas" . "arukas") ("Avi Vantage" . "avi") ("AWS" . "aws") ("Azure" . "azurerm") ("Azure Active Directory" . "azuread") ("Azure Stack" . "azurestack") ("Bitbucket" . "bitbucket") ("Brightbox" . "brightbox") ("CenturyLinkCloud" . "clc") ("Chef" . "chef") ("Circonus" . "circonus") ("Cisco ASA" . "ciscoasa") ("Cisco ACI" . "aci") ("Cloudflare" . "cloudflare") ("CloudScale.ch" . "cloudscale") ("CloudStack" . "cloudstack") ("Cobbler" . "cobbler") ("Consul" . "consul")
    ("Datadog" . "datadog") ("DigitalOcean" . "do") ("DNS" . "dns") ("DNSimple" . "dnsimple") ("DNSMadeEasy" . "dme") ("Docker" . "docker") ("Dyn" . "dyn") ("External" . "external") ("F5 BIG-IP" . "bigip") ("Fastly" . "fastly") ("FlexibleEngine" . "flexibleengine") ("FortiOS" . "fortios") ("GitHub" . "github") ("GitLab" . "gitlab") ("Google Cloud Platform" . "google") ("Grafana" . "grafana") ("Gridscale" . "gridscale") ("Hedvig" . "hedvig") ("Helm" . "helm") ("Heroku" . "heroku") ("Hetzner Cloud" . "hcloud") ("HTTP" . "http") ("HuaweiCloud" . "huaweicloud") ("Icinga2" . "icinga2")
    ("Ignition" . "ignition") ("InfluxDB" . "influxdb") ("JDCloud" . "jdcloud") ("Kubernetes" . "kubernetes") ("Librato" . "librato") ("Linode" . "linode") ("Local" . "local") ("Logentries" . "logentries") ("LogicMonitor" . "logicmonitor") ("Mailgun" . "mailgun") ("MySQL" . "mysql") ("Naver Cloud" . "ncloud") ("Netlify" . "netlify") ("New Relic" . "newrelic") ("Nomad" . "nomad") ("NS1" . "ns1") ("Null" . "null") ("Nutanix" . "nutanix") ("1 and 1" . "oneandone") ("OpenStack" . "openstack") ("OpenTelekomCloud" . "opentelekomcloud") ("OpsGenie" . "opsgenie")
    ("Oracle Cloud Infrastructure" . "oci") ("Oracle Cloud Platform" . "oraclepaas") ("Oracle Public Cloud" . "opc") ("OVH" . "ovh") ("Packet" . "packet") ("PagerDuty" . "pagerduty") ("Palo Alto Networks" . "panos") ("PostgreSQL" . "postgresql") ("PowerDNS" . "powerdns") ("ProfitBricks" . "profitbricks") ("RabbitMQ" . "rabbitmq") ("Rancher" . "rancher") ("Rancher2" . "rancher2") ("Random" . "random") ("RightScale" . "rightscale") ("Rundeck" . "rundeck") ("RunScope" . "runscope") ("Scaleway" . "scaleway") ("Selectel" . "selectel") ("SignalFx" . "signalfx") ("Skytap" . "skytap")
    ("SoftLayer" . "softlayer") ("Spotinst" . "spotinst") ("StatusCake" . "statuscake") ("TelefonicaOpenCloud" . "telefonicaopencloud") ("Template" . "template") ("TencentCloud" . "tencentcloud") ("Terraform" . "terraform") ("Terraform Enterprise" . "tfe") ("TLS" . "tls") ("Triton" . "triton") ("UCloud" . "ucloud") ("UltraDNS" . "ultradns") ("Vault" . "vault") ("VMware NSX-T" . "nsxt") ("VMware vCloud Director" . "vcd") ("VMware vRA7" . "vra7") ("VMware vSphere" . "vsphere") ("Yandex" . "yandex")))

;;;###autoload
(defun terraform-doc (&optional provider)
  "Lookup PROVIDER."
  (interactive (list
                (cdr (assoc (completing-read
                             "Provider: "
                             (mapcar (lambda (x) (car x)) terraform-doc-providers))
                            terraform-doc-providers))))
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
