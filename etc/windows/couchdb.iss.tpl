; Licensed under the Apache License, Version 2.0 (the "License"); you may not
; use this file except in compliance with the License. You may obtain a copy of
; the License at
;
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations under
; the License.

; CouchDB inno installer script
; %configure_input%

[Setup]
AppID=ApacheCouchDB
AppName=%package_name%
AppVerName=%package_name% %version%
AppPublisher=Apache Software Foundation
AppPublisherURL=http://couchdb.apache.org/
LicenseFile=../../LICENSE
DefaultDirName={pf}\Apache Software Foundation\CouchDB
DefaultGroupName=%package_name%
OutputBaseFilename=setup-couchdb-%version%
OutputDir=.

[Files]
Source: "%locallibbindir%\..\*.*"; DestDir: "{app}"; Flags: ignoreversion uninsrestartdelete restartreplace
; bin dir
Source: "%locallibbindir%\*.*"; DestDir: "{app}\bin"; Flags: ignoreversion uninsrestartdelete restartreplace recursesubdirs
; other dirs copied '*.*'
Source: "%locallibbindir%\..\erts-%erts_version%\*.*"; DestDir: "{app}\erts-%erts_version%"; Flags: ignoreversion uninsrestartdelete restartreplace recursesubdirs
Source: "%locallibbindir%\..\lib\*.*"; DestDir: "{app}\lib"; Flags: ignoreversion uninsrestartdelete restartreplace recursesubdirs
Source: "%locallibbindir%\..\share\*.*"; DestDir: "{app}\share"; Flags: ignoreversion uninsrestartdelete restartreplace recursesubdirs
Source: "%locallibbindir%\..\releases\*.*"; DestDir: "{app}\releases"; Flags: ignoreversion uninsrestartdelete restartreplace recursesubdirs
; skip ./usr, ./var

; custom stuff...
; ./etc/default.ini is unconditional
Source: "%locallibbindir%\..\etc\couchdb\default.ini"; DestDir: "{app}\etc\couchdb"; Flags: ignoreversion uninsrestartdelete restartreplace
; ./etc/local.ini is preserved and should not be updated if it exists
Source: "%locallibbindir%\..\etc\couchdb\local.ini"; DestDir: "{app}\etc\couchdb"; Flags: onlyifdoesntexist uninsneveruninstall
; readme
Source: "README.txt"; DestDir: "{app}"; Flags: isreadme

; msvc redists - see comments in configure.ac for notes about these...
; ( deleteafterinstall - not needed - {tmp} auto cleaned????
Source: "%msvc_redist_dir%\%msvc_redist_name%"; DestDir: "{tmp}"; Flags: deleteafterinstall

; These are erlang requirements and not copied by our makefiles.
Source: "%openssl_bin_dir%\ssleay32.dll"; DestDir: "{app}\bin"; Flags: ignoreversion uninsrestartdelete restartreplace
Source: "%openssl_bin_dir%\libeay32.dll"; DestDir: "{app}\bin"; Flags: ignoreversion uninsrestartdelete restartreplace

[Dirs]
Name: "{app}\var\lib\couchdb"; Permissions: authusers-modify
Name: "{app}\var\log\couchdb"; Permissions: authusers-modify
Name: "{app}\var\run\couchdb"; Permissions: authusers-modify
Name: "{app}\etc\couchdb"; Permissions: authusers-modify

[Icons]
Name: "{group}\Start CouchDB"; Filename: "{app}\bin\couchdb.bat"
Name: "{group}\Futon (CouchDB web interface)"; Filename: "http://127.0.0.1:5984/_utils"
Name: "{group}\CouchDB Web Site"; Filename: "http://couchdb.apache.org/"

[Tasks]
Name: service; Description: "Install couchdb as a Windows service"
Name: service\start; Description: "Start the service after installation"

[Run]
Filename: "{tmp}\%msvc_redist_name%"; Parameters: "/q"
; This is erlang's Install.exe which updates erl.ini correctly.
Filename: "{app}\Install.exe"; Parameters: "-s"; Flags: runhidden

; Commands for a service
; First attempt to nuke an existing service of this name, incase they are
; reinstalling without uninstalling
Filename: "{app}\erts-%erts_version%\bin\erlsrv.exe"; Parameters: "remove ""%package_name%"""; Flags: runhidden; Tasks: service
; add a new service, including automatic restart by default on failure
Filename: "{app}\erts-%erts_version%\bin\erlsrv.exe"; Parameters: "add ""%package_name%"" -workdir ""{app}\bin"" -onfail restart_always -args ""-sasl errlog_type error -s couch +A 4 +W w"" -comment ""%package_name% %version%"""; Flags: runhidden; Tasks: service
; and start it if requested
Filename: "{app}\erts-%erts_version%\bin\erlsrv.exe"; Parameters: "start ""%package_name%"""; Flags: runhidden; Tasks: service\start

[UninstallRun]
; erlsrv stops services prior to removing them
Filename: "{app}\erts-%erts_version%\bin\erlsrv.exe"; Parameters: "remove ""%package_name%"""; Flags: runhidden; Tasks: service
; kill epmd.exe if running to ensure uninstaller is not prevented from removing all binaries
Filename: "{app}\erts-%erts_version%\bin\epmd.exe"; Parameters: "-kill"; Flags: runhidden
