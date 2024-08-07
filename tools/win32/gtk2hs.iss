; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
AppName=Gtk2Hs
AppId=Gtk2Hs
AppVerName=Gtk2Hs 0.10.0
AppVersion=0.10.0
AppPublisher=The Gtk2Hs Team
AppPublisherURL=https://github.com/gtk2hs/
AppSupportURL=https://github.com/gtk2hs/gtk2hs
AppUpdatesURL=https://github.com/gtk2hs/gtk2hs

DefaultDirName=c:\gtk2hs\0.10.0
OutputBaseFilename=gtk2hs-0.10.0-win32-installer

VersionInfoVersion=0.10.0
VersionInfoCopyright=Copyright (C) 2001-2009 The Gtk2Hs Team

Compression=lzma/max
SolidCompression=yes

ChangesEnvironment=yes

PrivilegesRequired=none

[Components]
Name: "clibs";   Description: "Required C libraries"; Types: full compact custom; Flags: fixed
Name: "gtk2hs";  Description: "Gtk2Hs libraries for GHC 6.10.1"; Check: UseWithGhcVersion('6.10.1'); Types: full compact custom; Flags: fixed
Name: "doc";    Description: "API reference documentation"; Types: full
Name: "demo";   Description: "Source files for the Gtk2Hs demo programs"; Types: full

[Files]
Source: "gtk2hs-0.10.0-clibs\*";              DestDir: "{app}";       Components: clibs;   Flags: ignoreversion recursesubdirs createallsubdirs;
Source: "gtk2hs-0.10.0-ghc-6.10.1\*";          DestDir: "{app}";       Components: gtk2hs;  Flags: ignoreversion recursesubdirs createallsubdirs; AfterInstall: AfterPkgInstall;
Source: "gtk2hs-0.10.0-demo\*";               DestDir: "{app}"; Components: demo;   Flags: ignoreversion recursesubdirs createallsubdirs;
Source: "gtk2hs-0.10.0-doc\*";               DestDir: "{app}";  Components: doc;    Flags: ignoreversion recursesubdirs createallsubdirs;
Source: "COPYING.txt";                        DestDir: "{app}";                            Flags: ignoreversion;
Source: "AUTHORS.txt";                        DestDir: "{app}";                            Flags: ignoreversion;

[Registry]
Root: HKCU; Subkey: "Environment"; ValueName: "Path"; ValueType: "string"; ValueData: "{app}\bin;{olddata}"; Check: NotOnPathAlready(); Flags: preservestringtype;

[Run]
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\glib.package.conf""";       StatusMsg: "Registering glib package...";       Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\cairo.package.conf""";      StatusMsg: "Registering cairo package...";      Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\svgcairo.package.conf""";   StatusMsg: "Registering svgcairo package...";   Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\gtk.package.conf""";        StatusMsg: "Registering gtk package...";        Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\glade.package.conf""";      StatusMsg: "Registering glade package...";      Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\soegtk.package.conf""";     StatusMsg: "Registering soegtk package...";     Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\gtkglext.package.conf""";   StatusMsg: "Registering gtkglext package...";   Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\gtksourceview2.package.conf"""; StatusMsg: "Registering gtksourceview2 package..."; Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\gio.package.conf""";        StatusMsg: "Registering gio package...";        Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "update ""{app}\lib\gtk2hs\gstreamer.package.conf""";  StatusMsg: "Registering gstreamer package...";  Flags: runhidden

[UninstallRun]
Filename: "{code:ghcpkg}"; Parameters: "unregister gstreamer-0.10.0";  RunOnceId: "gstreamer"; Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister gio-0.10.0";        RunOnceId: "gio";  Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister gtksourceview2-0.10.0"; RunOnceId: "gtkglext";  Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister gtkglext-0.10.0";   RunOnceId: "gtkglext";  Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister soegtk-0.10.0";     RunOnceId: "soegtk";    Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister glade-0.10.0";      RunOnceId: "glade";     Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister gtk-0.10.0";        RunOnceId: "gtk";       Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister svgcairo-0.10.0";   RunOnceId: "svgcairo";  Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister cairo-0.10.0";      RunOnceId: "cairo";     Flags: runhidden
Filename: "{code:ghcpkg}"; Parameters: "unregister glib-0.10.0";       RunOnceId: "glib";      Flags: runhidden

[Code]
var
  GhcInstallDir: String;
  GhcInstallVersion: String;

  CheckingPage: TOutputProgressWizardPage;
  ErrorReportPage: TOutputMsgMemoWizardPage;
  ErrorContinueBox: TCheckBox;
  InstallationErrorCaption: String;
  InstallationErrorMessage: String;
  InstallationErrorDetail: String;

function ghcpkg(Param: String): String;
begin
  Result := AddBackslash(GhcInstallDir) + 'bin\ghc-pkg.exe';
end;

function NotOnPathAlready(): Boolean;
var
  BinDir, Path: String;
begin
  Log('Checking if Gtk2Hs\bin dir is already on the %PATH%');
  if RegQueryStringValue(HKEY_CURRENT_USER, 'Environment', 'Path', Path) then
  begin // Successfully read the value
    Log('HKCU\Environment\PATH = ' + Path);
    BinDir := ExpandConstant('{app}\bin');
    Log('Looking for Gtk2Hs\bin dir in %PATH%: ' + BinDir + ' in ' + Path);
    if Pos(LowerCase(BinDir), Lowercase(Path)) = 0 then
    begin
      Log('Did not find Gtk2Hs\bin dir in %PATH% so will add it');
      Result := True;
    end
    else
    begin
      Log('Found Gtk2Hs bin dir in %PATH% so will not add it again');
      Result := False;
    end
  end
  else // The key probably doesn't exist
  begin
    Log('Could not access HKCU\Environment\PATH so assume it is ok to add it');
    Result := True;
  end;
end;

{ exec a program and return its output }
function ExecOutput(const Filename, Params: String): String;
var
  TmpFile: String;
  ResultCode: Integer;
begin
  Result := '';
  TmpFile := GenerateUniqueName(ExpandConstant('{tmp}'), 'tmp');
  if Exec(ExpandConstant('{cmd}'), '/C ' + Filename + ' ' + Params + ' > ' + TmpFile,
     '', SW_HIDE, ewWaitUntilTerminated, ResultCode) then
    if ResultCode = 0 then
      if LoadStringFromFile(TmpFile, Result) then
      begin
        Result := Trim(Result);
        Log('ExecOutput: running ' + Filename + ' succeeded with output: ' + Result);
      end
      else
        Log('ExecOutput: running ' + Filename + ' succeeded, but the temp file was not created')
    else
      Log('ExecOutput: running ' + Filename + ' failed, code: ' + IntToStr(ResultCode))
  else
    { That's really odd, we should always be able to exec the command interpreter! }
    Log('ExecOutput: cannot exec command interpreter, code: ' + IntToStr(ResultCode) + ', message: ' + SysErrorMessage(ResultCode));
end;

function CheckGhcVersionIsOk(const Path: String; var Version: String):Boolean;
begin
  Version := ExecOutput(AddBackslash(Path) + 'bin\ghc.exe', '--numeric-version');
  GhcInstallVersion := Version;
  
  Result := (Version = '6.10.1');
end;

var
  DetectValidGhcInstallationCached: Boolean;
  DetectValidGhcInstallationResult: Boolean;

function DetectValidGhcInstallation(): Boolean;
var
  HaveSomeGHCInstalled: Boolean;
  GHCVersion: String;

begin
if DetectValidGhcInstallationCached then
  Result := DetectValidGhcInstallationResult
else
begin
  Result := False;

  { first check for ghc on the path, then look in the registry }
  
  begin
    GhcInstallDir := ExecOutput('ghc.exe', '--print-libdir');
    if GhcInstallDir <> '' then
    begin
      StringChange(GhcInstallDir, '/', '\');
      Log('DetectValidGhcInstallation: found ghc on %PATH% with libdir: ' + GhcInstallDir);
      HaveSomeGHCInstalled := True;
      Result := CheckGhcVersionIsOk(GhcInstallDir, GHCVersion);
    end;
  end;

  if (Result = False) and RegKeyExists(HKEY_CURRENT_USER, 'Software\Haskell\GHC') then
  begin
    Log('DetectValidGhcInstallation: found HKCU\Software\Haskell\GHC');
    HaveSomeGHCInstalled := True;
    RegQueryStringValue(HKEY_CURRENT_USER, 'Software\Haskell\GHC\ghc-6.10.1', 'InstallDir', GhcInstallDir);
    Result := CheckGhcVersionIsOk(GhcInstallDir, GHCVersion);
  end;
  if (Result = False) and RegKeyExists(HKEY_LOCAL_MACHINE, 'SOFTWARE\Haskell\GHC') then
  begin
    Log('DetectValidGhcInstallation: found HKLM\SOFTWARE\Haskell\GHC');
    HaveSomeGHCInstalled := True;
    RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Haskell\GHC\ghc-6.10.1', 'InstallDir', GhcInstallDir);
    Result := CheckGhcVersionIsOk(GhcInstallDir, GHCVersion);
  end;
  if Result then
    Log('DetectValidGhcInstallation: correct version of ghc found at: ' + GhcInstallDir)
  else if HaveSomeGHCInstalled and (GHCVersion <> '') then
  begin
    Log('DetectValidGhcInstallation: incorrect ghc version installed: ' + GHCVersion);
    InstallationErrorCaption := 'The version of GHC currently installed is not suitable.';
    InstallationErrorMessage := 'This version of Gtk2Hs requires GHC version 6.10.1.' #13#10 #13#10
                                'Setup found GHC version ' + GHCVersion + ' installed in the folder:' #13#10
                              + GhcInstallDir
  end
  else if HaveSomeGHCInstalled and (GhcInstallDir <> '') then
  begin
    Log('DetectValidGhcInstallation: some non-working version of ghc appears to be installed at: ' + GhcInstallDir);
    InstallationErrorCaption := 'GHC does not seem to be working.';
    InstallationErrorMessage := 'GHC does not appear to be installed correctly, try reinstalling GHC version 6.10.1' #13#10 #13#10
                                'Setup found what appears to be a non-working installation of GHC in the folder:' #13#10
                              + GhcInstallDir
  end
  else if HaveSomeGHCInstalled then
  begin
    Log('DetectValidGhcInstallation: corrupted ghc installation detected, probably messed up registry keys');
    InstallationErrorCaption := 'Setup did not find GHC on your computer.';
    InstallationErrorMessage := 'GHC does not appear to be installed (or the installation is corrupted), please install GHC version 6.10.1';
  end
  else
  begin
    Log('DetectValidGhcInstallation: no installation of ghc detected');
    InstallationErrorCaption := 'Setup did not find GHC on your computer.';
    InstallationErrorMessage := 'Gtk2Hs requires GHC to be installed first, please install GHC version 6.10.1' #13#10 #13#10
                                'If you installed GHC manually then make sure ghc.exe is on the path.';
  end;
  
  If not Result then
  begin
    Log(InstallationErrorCaption);
    Log(InstallationErrorMessage);
  end;

  DetectValidGhcInstallationResult := Result;
  DetectValidGhcInstallationCached := True;
end;
end;

function UseWithGhcVersion(const GhcVersion: String): Boolean;
begin
  DetectValidGhcInstallation();
  Result := (GhcVersion = GhcInstallVersion);
end;

var
  ChecksOk : Boolean;

function ShouldSkipPage(CurPageID: Integer): Boolean;
begin
  Log('ShouldSkipPage: CurPageID = ' + IntToStr(CurPageID));
  if CurPageID = ErrorReportPage.ID then
  begin
    if ChecksOk then
      Log('ChecksOk = True')
    else
      Log('ChecksOk = False');
    Result := ChecksOk
  end
  else
    Result := False;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
var
  DllFiles, Offenders: array of String;
  Path, Offender: String;
  N, M, NumFiles: Integer;
begin
  Log('NextButtonClick: CurPageID = ' + IntToStr(CurPageID));
  if CurPageID = wpWelcome then
  begin
    if not DetectValidGhcInstallation() then
      ChecksOk := False
    else
    begin
      DllFiles := [
        'charset.dll',
        'gconftool-2.exe',
        'gdk-pixbuf-csource.exe',
        'gdk-pixbuf-query-loaders.exe',
        'glib-genmarshal.exe',
        'gobject-query.exe',
        'gspawn-win32-helper-console.exe',
        'gspawn-win32-helper.exe',
        'gst-inspect-0.10.exe',
        'gst-launch-0.10.exe',
        'gtk-demo.exe',
        'gtk-query-immodules-2.0.exe',
        'gtk-update-icon-cache.exe',
        'iconv.dll',
        'intl.dll',
        'ior-decode-2.exe',
        'jpeg62.dll',
        'libatk-1.0-0.dll',
        'libcairo-2.dll',
        'libgailutil-18.dll',
        'libgconf-2-4.dll',
        'libgdk-win32-2.0-0.dll',
        'libgdkglext-win32-1.0-0.dll',
        'libgdk_pixbuf-2.0-0.dll',
        'libgio-2.0-0.dll',
        'libglade-2.0-0.dll',
        'libglib-2.0-0.dll',
        'libgmodule-2.0-0.dll',
        'libgobject-2.0-0.dll',
        'libgstaudio-0.10.dll',
        'libgstbase-0.10.dll',
        'libgstcdda-0.10.dll',
        'libgstcontroller-0.10.dll',
        'libgstdataprotocol-0.10.dll',
        'libgstfft-0.10.dll',
        'libgstinterfaces-0.10.dll',
        'libgstnet-0.10.dll',
        'libgstnetbuffer-0.10.dll',
        'libgstpbutils-0.10.dll',
        'libgstreamer-0.10.dll',
        'libgstriff-0.10.dll',
        'libgstrtp-0.10.dll',
        'libgstrtsp-0.10.dll',
        'libgstsdp-0.10.dll',
        'libgsttag-0.10.dll',
        'libgstvideo-0.10.dll',
        'libgthread-2.0-0.dll',
        'libgtk-win32-2.0-0.dll',
        'libgtkglext-win32-1.0-0.dll',
        'libgtksourceview-2.0-0.dll',
        'libORBit-2-0.dll',
        'libORBit-imodule-2-0.dll',
        'libORBitCosNaming-2-0.dll',
        'libpango-1.0-0.dll',
        'libpangocairo-1.0-0.dll',
        'libpangoft2-1.0-0.dll',
        'libpangowin32-1.0-0.dll',
        'libpng12-0.dll',
        'librsvg-2-2.dll',
        'libtiff3.dll',
        'libxml2.dll',
        'orbit-idl-2.exe',
        'pkg-config.exe',
        'rsvg-convert.exe',
        'rsvg-view.exe',
        'typelib-dump.exe',
        'zlib1.dll',
        'libgconfbackend-oldxml.dll',
        'libgconfbackend-xml.dll',
        'libgstadder.dll',
        'libgstaudioconvert.dll',
        'libgstaudiorate.dll',
        'libgstaudioresample.dll',
        'libgstaudiotestsrc.dll',
        'libgstcoreelements.dll',
        'libgstdecodebin.dll',
        'libgstdecodebin2.dll',
        'libgstffmpegcolorspace.dll',
        'libgstgdp.dll',
        'libgstogg.dll',
        'libgstplaybin.dll',
        'libgsttheora.dll',
        'libgsttypefindfunctions.dll',
        'libgstvideorate.dll',
        'libgstvideoscale.dll',
        'libgstvideotestsrc.dll',
        'libgstvolume.dll',
        'libgstvorbis.dll',
        'libpixmap.dll',
        'libwimp.dll',
        'libpixbufloader-ani.dll',
        'libpixbufloader-bmp.dll',
        'libpixbufloader-gif.dll',
        'libpixbufloader-icns.dll',
        'libpixbufloader-ico.dll',
        'libpixbufloader-jpeg.dll',
        'libpixbufloader-pcx.dll',
        'libpixbufloader-png.dll',
        'libpixbufloader-pnm.dll',
        'libpixbufloader-ras.dll',
        'libpixbufloader-tga.dll',
        'libpixbufloader-tiff.dll',
        'libpixbufloader-wbmp.dll',
        'libpixbufloader-xbm.dll',
        'libpixbufloader-xpm.dll',
        'libgail.dll',
        'gconfd-2.exe'
        ];
      NumFiles := GetArrayLength(DllFiles);
      SetArrayLength(Offenders, NumFiles);

      RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', 'Path', Path);
      Path := GetSystemDir() + ';' + GetWinDir() + ';' + Path;
      
      Log('Checking DLL path: ' + Path);

      CheckingPage.SetText('Checking Gtk+ DLL search path...', '');
      CheckingPage.SetProgress(0, NumFiles);
      CheckingPage.Show;
      try
        M := 0;
        for N := 0 to NumFiles - 1 do
        begin
          Offender := FileSearch(DllFiles[N], Path);
          if Offender <> '' then
          begin
            Log('Found clashing dll/exe file at ' + Offender);
            Offenders[M] := Offender;
            M := M + 1;
          end
          CheckingPage.SetProgress(N+1, NumFiles);
          Sleep(10);
        end;
        if M > 0 then
        begin
          ChecksOk := False;
          InstallationErrorCaption := 'Setup found clashing DLL files on the search path.';
          InstallationErrorMessage := 'Please read the following description of the problem and how to fix it:';

          InstallationErrorDetail :=
              'For Haskell Gtk programs to work properly, the Gtk+ DLL files need to be in the DLL '
            + 'search path. However, the following DLLs files that Gtk+ uses are already on the '
            + 'system-wide search path:' #13#10

          for N := 0 to M - 1 do
            InstallationErrorDetail := InstallationErrorDetail + #13#10 + Offenders[N];

          InstallationErrorDetail := InstallationErrorDetail + #13#10 #13#10
              'The problem is that when you run a Haskell Gtk program these DLLs would get used '
            + 'rather than the proper versions of the Gtk+ DLLs installed by this setup program.'
            + 'This would almost certainly cause all your Haskell Gtk programs to not work.' #13#10 #13#10
              'You can fix this problem in a couple of ways:' #13#10 #13#10
              '1) You could uninstall or delete the program that installed the offending files '
            + '(eg it might be that you have an old version of Gtk+ installed).' #13#10 #13#10
              '2) You could modify the system-wide "Path" environment variable and remove the '
            + 'offending directory from it.'
        end
        else
          ChecksOk := True;
      finally
        CheckingPage.Hide;
      end;
    end;
  end
  Result := True;
end;

procedure PackageFileVarSubstitute(const PackageFile: String);
var
  PackageFileContent: String;
  BaseDir: String;
  PackageLibDir: String;
begin
  BaseDir := ExpandConstant('{app}');
  PackageLibDir := BaseDir + '\lib\gtk2hs';
  StringChange(BaseDir, '\', '/');
  StringChange(PackageLibDir, '\', '/');
  
  LoadStringFromFile(PackageFile, PackageFileContent);

  StringChange(PackageFileContent, '${pkglibdir}', PackageLibDir);
  StringChange(PackageFileContent, '${GTK_BASEPATH}', BaseDir);
  
  SaveStringToFile(PackageFile, PackageFileContent, False);
  Log('Expanding variables in ' + PackageFile);
end;

procedure AfterPkgInstall;
begin
  if (ExtractFileExt(CurrentFileName) = '.conf') then
    PackageFileVarSubstitute(ExpandConstant(CurrentFileName));
end;

// Detect when the ready info page gets displayed
function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
                         MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo,
                         MemoTasksInfo: String): String;
begin
		Result := MemoDirInfo + NewLine
		        + NewLine
		        +	'Using GHC ' + GhcInstallVersion + ' installed at:' + NewLine
		        + Space + GhcInstallDir + NewLine
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  BinDir, Path: String;
begin
  if (CurUninstallStep = usPostUninstall)
     and (RegQueryStringValue(HKEY_CURRENT_USER, 'Environment', 'PATH', Path)) then
  begin
    BinDir := ExpandConstant('{app}\bin');
    if Pos(LowerCase(BinDir) + ';', Lowercase(Path)) <> 0 then
    begin
      StringChange(Path, BinDir + ';', '');
      RegWriteStringValue(HKEY_CURRENT_USER, 'Environment', 'PATH', Path);
    end;
  end;
end;

procedure ErrorReportPageNotify(Sender: TWizardPage);
begin
  WizardForm.NextButton.Enabled := ErrorContinueBox.Checked;
  ErrorReportPage.Description := InstallationErrorCaption;
  ErrorReportPage.SubCaptionLabel.Caption := InstallationErrorMessage;
  if InstallationErrorDetail = '' then
    ErrorReportPage.SubCaptionLabel.Height := 50
  else
  begin
    ErrorReportPage.RichEditViewer.RTFText := InstallationErrorDetail
    ErrorReportPage.RichEditViewer.Show();
    ErrorContinueBox.Show();
  end;
end;

procedure ErrorContinueClicked(Sender: TObject);
begin
  WizardForm.NextButton.Enabled := ErrorContinueBox.Checked;
end;

procedure InitializeWizard();
begin
  CheckingPage := CreateOutputProgressPage('Checking requirements', 'Checking for GHC and Gtk+');

  ErrorReportPage := CreateOutputMsgMemoPage(wpWelcome, 'Setup cannot continue', 'GHC or Gtk+ problem', 'Your installation of GHC or Gtk+ is messed up!', 'Foo bar!');
  ErrorReportPage.RichEditViewer.Height := ErrorReportPage.RichEditViewer.Height - ScaleY(22);
  ErrorReportPage.OnActivate := @ErrorReportPageNotify;
  ErrorReportPage.RichEditViewer.Hide();

  ErrorContinueBox := TCheckBox.Create(ErrorReportPage);
  ErrorContinueBox.Parent := ErrorReportPage.Surface;
  ErrorContinueBox.Top := ErrorReportPage.RichEditViewer.Height + ErrorReportPage.RichEditViewer.Top + ScaleY(8);
  ErrorContinueBox.Width  := ErrorReportPage.RichEditViewer.Width;
  ErrorContinueBox.Caption := 'Continue anyway. (You will have to fix the DLL search path problem yourself later.)';
  ErrorContinueBox.Checked := False;
  ErrorContinueBox.OnClick := @ErrorContinueClicked;
end;

