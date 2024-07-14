;;; terraform-doc.el --- Look up terraform documentation on the fly -*- lexical-binding: t -*-

;; Copyright (C) 2019 Giap Tran <txgvnn@gmail.com>

;; Author: Giap Tran <txgvnn@gmail.com>
;; URL: https://github.com/TxGVNN/terraform-doc
;; Version: 2.0.1
;; Package-Requires: ((emacs "25.1") (request "0.3.0") (promise "1.1") (org "9.2"))
;; Keywords: comm, docs, tools, terraform

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

(require 'request)
(require 'promise)
(require 'cl-lib)
(require 'org)

(defgroup terraform nil
  "Major mode of `terraform-doc' file."
  :group 'languages
  :prefix "terraform-doc-")

(defcustom terraform-doc-markdown-mode-function 'markdown-mode
  "Function to use for markdown rendering, e.g. `markdown-mode' or `gfm-view-mode'."
  :type 'function
  :group 'terraform)

(defvar terraform-doc-providers
  '("1Password/onepassword" "AppViewX/appviewx" "AviatrixSystems/aviatrix" "Azure/alz" "Azure/azapi"
    "Azure/modtm" "Backblaze/b2" "BetterStackHQ/better-uptime" "BetterStackHQ/logtail" "CheckPointSW/checkpoint"
    "CheckPointSW/infinity-next" "CiscoDevNet/aci" "CiscoDevNet/catalystcenter" "CiscoDevNet/cdo"
    "CiscoDevNet/ciscoasa" "CiscoDevNet/ciscomcd" "CiscoDevNet/cml2" "CiscoDevNet/dcnm" "CiscoDevNet/fmc"
    "CiscoDevNet/intersight" "CiscoDevNet/iosxe" "CiscoDevNet/iosxr" "CiscoDevNet/ise" "CiscoDevNet/mso"
    "CiscoDevNet/nso" "CiscoDevNet/nxos" "CiscoDevNet/sdwan" "CiscoDevNet/secureworkload"
    "CiscoDevNet/tetration" "ClickHouse/clickhouse" "Commvault/commvault" "ConsenSys/quorum"
    "Constellix/constellix" "CrunchyData/crunchybridge" "DNSMadeEasy/dme" "DataDog/datadog"
    "DevCycleHQ/devcycle" "EnterpriseDB/biganimal" "F5Networks/bigip" "F5Networks/f5os" "Flagsmith/flagsmith"
    "G-Core/gcore" "G-Core/gcorelabs" "HPE/hpegl" "HewlettPackard/oneview" "Icinga/icinga2" "Juniper/apstra"
    "Juniper/junos-vsrx" "JupiterOne/jupiterone" "Keeper-Security/keeper" "Keeper-Security/secretsmanager"
    "LumenTech/lumen" "MaterializeInc/materialize" "MyPureCloud/genesyscloud" "NaverCloudPlatform/ncloud"
    "NetApp/netapp-cloudmanager" "NetApp/netapp-elementsw" "NetApp/netapp-gcp" "NetApp/netapp-ontap"
    "OctopusDeployLabs/octopusdeploy" "OpenNebula/opennebula" "OpsLevel/opslevel" "PacketFabric/packetfabric"
    "PagerDuty/pagerduty" "PaloAltoNetworks/bridgecrew" "PaloAltoNetworks/cloudngfwaws" "PaloAltoNetworks/panos"
    "PaloAltoNetworks/prismacloud" "PaloAltoNetworks/prismacloud-waas" "PaloAltoNetworks/prismacloudcompute"
    "PaloAltoNetworks/scm" "PrefectHQ/prefect" "PureStorage-OpenConnect/cbs" "PureStorage-OpenConnect/fusion"
    "RafaySystems/rafay" "RedisLabs/rediscloud" "SUSE/susepubliccloud" "SamsungSDSCloud/samsungcloudplatform"
    "SatoriCyber/satori" "ScpDevTerra/scp" "Seagate/lyvecloud" "Seeweb/seeweb" "Snowflake-Labs/snowflake"
    "StatusCakeDev/statuscake" "SumoLogic/sumologic" "SyntropyNet/syntropystack" "ThalesGroup/ciphertrust"
    "Traceableai/awsapigateway" "Traceableai/lambdabased" "UpCloudLtd/upcloud" "Venafi/venafi"
    "Venafi/venafi-token" "a10networks/thunder" "a10networks/vthunder" "abbeylabs/abbey" "ably/ably"
    "accuknox/accuknox" "advancedhosting/ah" "airbytehq/airbyte" "aiven/aiven" "akamai/akamai"
    "alertmixer/amixr" "aliyun/alibabacloudstack" "aliyun/alicloud" "alkiranet/alkira" "anexia-it/anxcloud"
    "aquasecurity/aquasec" "ariga/atlas" "aristanetworks/cloudeos" "aristanetworks/cloudvision" "aruba/aoscx"
    "assistanz/stackbill" "auth0/auth0" "aztfmod/azurecaf" "barracudanetworks/barracudawaf"
    "bluecatlabs/bluecat" "brightbox/brightbox" "britive/britive" "buddy/buddy" "buildkite/buildkite"
    "castai/castai" "chainguard-dev/apko" "chainguard-dev/chainguard" "chainguard-dev/cosign"
    "chainguard-dev/imagetest" "chainguard-dev/ko" "chainguard-dev/oci" "checkly/checkly"
    "circonus-labs/circonus" "citrix/citrix" "citrix/citrixadc" "civo/civo" "cloudera/cdp"
    "cloudflare/cloudflare" "cloudscale-ch/cloudscale" "cloudsigma/cloudsigma" "cloudsmith-io/cloudsmith"
    "cloudtamer-io/cloudtamerio" "clumio-code/clumio" "cockroachdb/cockroach" "codefresh-io/codefresh"
    "coder/coder" "cohesity/cohesity" "cohesive/cohesivenet" "configcat/configcat" "confluentinc/confluent"
    "controlplane-com/cpln" "coralogix/coralogix" "couchbasecloud/couchbase-capella"
    "couchbasecloud/couchbasecapella" "databricks/databricks" "datastax/astra" "dell/apex" "dell/ome"
    "dell/powerflex" "dell/powermax" "dell/powerscale" "dell/powerstore" "dell/redfish"
    "delphix-integrations/delphix" "digitalocean/digitalocean" "dnsimple/dnsimple" "dome9/dome9"
    "dynatrace-oss/dynatrace" "elastic/ec" "elastic/elasticstack" "equinix/equinix" "equinix/metal"
    "exoscale/exoscale" "fastly/fastly" "firehydrant/firehydrant" "fivetran/fivetran" "fortanix/dsm"
    "fortinetdev/fortiadc" "fortinetdev/fortianalyzer" "fortinetdev/fortiflexvm" "fortinetdev/fortimanager"
    "fortinetdev/fortios" "fortinetdev/fortiswitch" "genesiscloud/genesiscloud" "gitlabhq/gitlab"
    "gocachebr/gocache" "grafana/grafana" "gridscale/gridscale" "harness/harness" "hashicorp/ad"
    "hashicorp/archive" "hashicorp/aws" "hashicorp/awscc" "hashicorp/azuread" "hashicorp/azurerm"
    "hashicorp/azurestack" "hashicorp/boundary" "hashicorp/cloudinit" "hashicorp/consul" "hashicorp/dns"
    "hashicorp/external" "hashicorp/google" "hashicorp/google-beta" "hashicorp/googleworkspace" "hashicorp/hcp"
    "hashicorp/hcs" "hashicorp/helm" "hashicorp/http" "hashicorp/kubernetes" "hashicorp/local" "hashicorp/nomad"
    "hashicorp/null" "hashicorp/opc" "hashicorp/oraclepaas" "hashicorp/random" "hashicorp/salesforce"
    "hashicorp/template" "hashicorp/tfe" "hashicorp/time" "hashicorp/tls" "hashicorp/vault" "hashicorp/vsphere"
    "hashicorp/waypoint" "heroku/heroku" "hetznercloud/hcloud" "hivelocity/hivelocity" "honeycombio/honeycombio"
    "hundio/hund" "iLert/ilert" "illumio/illumio-core" "indykite/indykite" "infobloxopen/b1ddi"
    "infobloxopen/infoblox" "infracost/infracost" "infrahq/infra" "instaclustr/instaclustr"
    "integrations/github" "ionos-cloud/ionoscloud" "ionos-cloud/profitbricks" "ionos-developer/ionosdeveloper"
    "iterative/iterative" "itglobalcom/serverspace" "jfrog/artifactory" "jfrog/pipeline" "jfrog/platform"
    "jfrog/project" "jfrog/xray" "joyent/triton" "kaleido-io/kaleido" "kentik/kentik-cloudexport"
    "kentik/kentik-synthetics" "kestra-io/kestra" "keyfactor-pub/keyfactor" "kionsoftware/kion" "koyeb/koyeb"
    "lacework/lacework" "latitudesh/latitudesh" "launchdarkly/launchdarkly" "lightstep/lightstep"
    "linode/linode" "llnw/limelight" "logicmonitor/logicmonitor" "logzio/logzio" "mariadb-corporation/skysql"
    "mariadb-corporation/skysql-beta" "metalsoft-io/metalcloud" "microsoft/azuredevops" "mondoohq/mondoo"
    "mongodb/mongodbatlas" "multycloud/multy" "netactuate/netactuate" "netooze/netooze" "netrisai/netris"
    "netskopeoss/netskope" "netskopeoss/netskopebwan" "newrelic/newrelic" "ngrok/ngrok" "nhn-cloud/nhncloud"
    "nirmata/nirmata" "nobl9/nobl9" "ns1-terraform/ns1" "nttcom/ecl" "nttcom/fic" "nullstone-io/dockerless"
    "nullstone-io/ns" "nutanix/nutanix" "nutanix/nutanixkps" "okta/okta" "oktadeveloper/oktaasa"
    "onelogin/onelogin" "opalsecurity/opal" "oracle/oci" "orcasecurity/orcasecurity" "outscale/outscale"
    "ovh/ovh" "panther-labs/panther" "phoenixnap/pnap" "pingidentity/davinci" "pingidentity/pingdirectory"
    "pingidentity/pingfederate" "pingidentity/pingone" "prosimo-io/prosimo" "rackn/drp" "rancher/rancher2"
    "rancher/rke" "rockset/rockset" "rollbar/rollbar" "rubrikinc/polaris" "rundeck/rundeck" "scaleway/scaleway"
    "sematext/sematext" "shipa-corp/shipa" "shorelinesoftware/shoreline" "siderolabs/talos"
    "signalsciences/sigsci" "singlestore-labs/singlestoredb" "site24x7/site24x7" "skytap/skytap"
    "snyk-terraform-assets/snyk" "spectrocloud/spectrocloud" "splunk/artifacts" "splunk/splunk"
    "splunk/splunkconfig" "splunk/synthetics" "splunk/victorops" "splunk-terraform/signalfx" "spotinst/spotinst"
    "squaredup/squaredup" "stackpath/stackpath" "streamkap-com/streamkap" "strongdm/sdm" "symopsio/sym"
    "sysdiglabs/sysdig" "temporalio/temporalcloud" "tencentcloudstack/tencentcloud" "terraform-redhat/rhcs"
    "thousandeyes/thousandeyes" "tidbcloud/tidbcloud" "timeplus-io/timeplus" "timescale/timescale"
    "transloadit/transloadit" "turbot/pipes" "turbot/steampipecloud" "turbot/turbot" "ultradns/ultradns"
    "valtix-security/valtix" "vantage-sh/vantage" "vercel/vercel" "versa-networks/versadirector" "vmware/avi"
    "vmware/nsxt" "vmware/nsxt-virtual-private-cloud" "vmware/tanzu-mission-control" "vmware/vcd" "vmware/vcda"
    "vmware/vcf" "vmware/vmc" "vmware/vra" "vmware/vra7" "vmware/wavefront" "vmware-tanzu/carvel"
    "volterraedge/volterra" "vultr/vultr" "wallix/waapm" "zededa/zedcloud" "zenlayer/zenlayercloud"
    "zentralopensource/zentral" "zerotier/zerotier" "zitadel/zitadel" "zscaler/zia" "zscaler/zpa"))

(defun terraform-doc--get-version(full-name)
  "Get tags of the provider FULL-NAME."
  ;; { "type": "provider-versions","id": "32928","attributes": { "version": "v2.4.0"}}
  ;; return a set of (tag . id)
  (mapcar
   (lambda (x)
     (cons
      (cdr (assoc 'version (cdr (assoc 'attributes x))))
      (cdr (assoc 'id x))))
   (with-temp-buffer
     (url-insert-file-contents (format "https://registry.terraform.io/v2/providers/%s?include=provider-versions" full-name))
     (cdr (assoc 'included (json-read))))))

;;;###autoload
(defun terraform-doc (&optional provider version)
  "Look up PROVIDER@VERSION."
  (interactive (list
                (completing-read
                 "Provider: "
                 terraform-doc-providers)
                nil))
  (let ((version-list (terraform-doc--get-version provider)))
    (unless version
      (setq version (completing-read "Version: " version-list nil nil nil nil (car version-list))))
    (terraform-doc--render-tree provider version (cdr (assoc version version-list)))))

(defun terraform--prepare-buffer (buf &optional msg)
  "Print MSG message and prepare window for BUF buffer."
  (when (not (equal (window-buffer) buf))
    (switch-to-buffer-other-window buf))
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert (if msg msg "Loading..."))) buf)

(defun terraform-doc--render-tree (provider version id)
  "Render the PROVIDER@VERSION(ID)."
  (let ((url (format "https://registry.terraform.io/v2/provider-versions/%s?include=provider-docs" id))
        (buffer-name (format "*Terraform:%s@%s*" provider version)))
    (terraform--prepare-buffer (get-buffer-create buffer-name))
    (promise-chain (terraform--promise-dom url)
      (then (lambda (result)
              (terraform--print-frontpage result provider version)))
      (promise-catch (lambda (reason)
                       (message "catch error in promise prontpage: %s" reason))))))


(defun terraform--promise-dom (url)
  "Promise (url . dom) from URL with curl."
  (promise-new
   (lambda (resolve reject)
     (request url
       :parser (lambda () (json-read))
       :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (funcall reject  error-thrown)))
       :success (cl-function (lambda (&key data &allow-other-keys)
                               (funcall resolve data)))))))

(defun terraform--print-frontpage-item (thing)
  "Print THING of PROVIDER."
  (let* ((provider (cdr (assoc 'provider thing)))
         (version (cdr (assoc 'version thing)))
         ;; (title (cdr (assoc 'title (cdr (assoc 'attributes thing)))))
         (path (cdr (assoc 'path (cdr (assoc 'attributes thing)))))
         (url (format "https://registry.terraform.io/v2/provider-docs/%s" (cdr (assoc 'id thing)))))
    (insert (format "[[elisp:(terraform-doc--render-object \"%s\" \"*Terraform:%s@%s:%s\")][%s]]\n"
                    url
                    provider version path
                    path))))

(defun terraform-doc--render-object (url buffer-name)
  "Render data of URL into BUFFER-NAME buffer."
  (terraform--prepare-buffer buffer-name)
  (promise-chain (terraform--promise-dom url)
    (then (lambda (result)
            (let ((buf (get-buffer-create buffer-name)))
              (with-current-buffer buf
                (read-only-mode -1)
                (erase-buffer)
                (insert (format "%s" (cdr (assoc 'content (cdr (assoc 'attributes (cdr (assoc 'data result))))))))
                (if (fboundp terraform-doc-markdown-mode-function)
                    (funcall terraform-doc-markdown-mode-function))
                (switch-to-buffer buf)))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise prontpage: %s" reason)))))

(defun terraform--print-frontpage (dom provider version)
  "Parse DOM and render on buffer of PROVIDER@VERSION."
  (with-current-buffer (get-buffer-create (format "*Terraform:%s@%s*" provider version))
    (read-only-mode -1)
    (erase-buffer)
    (let* ((items (cdr (assoc 'included dom)))
           ;; push provider and version to data element
           (data (mapcar (lambda (x) (append x (list (cons 'provider provider) (cons 'version version)))) items)))
      (cl-mapcar #'terraform--print-frontpage-item data))
    (org-mode)
    (setq-local org-link-elisp-confirm-function nil)
    (goto-char (point-min))))

(provide 'terraform-doc)
;;; terraform-doc.el ends here
