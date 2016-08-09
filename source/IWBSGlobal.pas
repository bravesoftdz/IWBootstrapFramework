unit IWBSGlobal;

interface

uses Classes, SysUtils, IWHTMLTag, IWBSLayoutMgr, System.SyncObjs;

 type

   TIWBSGlobal = class  //Global vars in this class are Thread Safe
     strict private
      class var FExclusiveWriteLinkFiles:TMultiReadExclusiveWriteSynchronizer;
      {For adding global files as needed.
      Example plugin Mask is added only if the Mask property is active in the input component.
      This decreases loading files on the page}
      class var FgIWBSLinkFiles:TStringList;
     private
      class procedure Initialize;
      class procedure Finalize;
     public
     class function IWBSLinkFiles:TStringList;
     class procedure IWBSAddGlobalLinkFile(const AFile: string);
     class procedure IWBSRemoveGlobalLinkFile(const AFile: string);
   end;




// ATENTION!!!!, Global files should only be modified in TIWServerController.OnConfig event, they are not Thread Safe.
// only boolean vars (for enable/disable plugins) could be changed anytime, because boleans vars work with attomic read/write
//
var
  // paths to framework JS and CSS files
  gIWBSLibPath: string = '/iwbs/';

  // path for JQuery library (required)
  gIWBSLibJQueryJs: string = '/<iwbspath>/jquery-1.12.1.min.js';

  // path for bootstrap library (required)
  gIWBSLibBootstrapCss: string = '/<iwbspath>/bootstrap-3.3.6/css/bootstrap.min.css';
  gIWBSLibBootstrapJs: string = '/<iwbspath>/bootstrap-3.3.6/js/bootstrap.min.js';

  // path for iwbs support files (required)
  gIWBSLibIWBSCss: string = '/<iwbspath>/iwbs.css';
  gIWBSLibIWBSJs: string = '/<iwbspath>/iwbs.js';

  // path for Polyfiller library, it provides full support for html5 to older browsers (optional)
  gIWBSLibPolyFiller: boolean = True;
  gIWBSLibPolyFillerJs: string = '/<iwbspath>/webshim-1.15.10/js-webshim/minified/polyfiller.js';

  // path for Dynamic Tabs plugin (optional)
  gIWBSLibDynamicTabs: boolean = False;
  gIWBSLibDynamicTabsCss: string = '/<iwbspath>/dyntabs/bootstrap-dynamic-tabs.css';
  gIWBSLibDynamicTabsJs: string = '/<iwbspath>/dyntabs/bootstrap-dynamic-tabs.js';

  // path for Summernote plugin (optional)
  gIWBSLibSummerNote: boolean = False;
  gIWBSLibSummerNoteCss: string = '/<iwbspath>/summernote/dist/summernote.css';
  gIWBSLibSummerNoteJs: string = '/<iwbspath>/summernote/dist/summernote.js';

  // configurations for design time grid
  gIWBSRenderingSortMethod: TIWBSRenderingSortMethod = bsrmSortYX;
  gIWBSRenderingGridPrecision: integer = 12;

  // server start timestamp value in format string, it is usefull to force client refresh cache browsers of included files
  gIWBSRefreshCacheParam: string = '';

  // occurs for each control, after component is changed on an Asyn call, it doesn't occurs if the control is fully rendered
  // useful for execute a refresh on third party plugins that need it
  gIWBSOnAfterAsyncChange: procedure(AControl: TComponent; const AHTMLName);

  // occurs for each control, after control is rendered
  gIWBSOnAfterRender: procedure(AControl: TComponent);

  // occurs for each control, before control is rendered
  // useful for set common properties to certain components, for examplë: to enable third party plugins
  gIWBSOnRender: procedure(AControl: TComponent);

  //Global BootsTrap css file Theme for all Forms
  gIWBSGlobalThemeCss:string = '';


implementation


class procedure TIWBSGlobal.Finalize;
begin
   TIWBSGlobal.FExclusiveWriteLinkFiles.Free;
  FreeAndNil(TIWBSGlobal.FgIWBSLinkFiles);
end;


class procedure TIWBSGlobal.Initialize;
begin
  TIWBSGlobal.FExclusiveWriteLinkFiles:= TMultiReadExclusiveWriteSynchronizer.Create;
  TIWBSGlobal.FgIWBSLinkFiles:= nil;
end;

class procedure TIWBSGlobal.IWBSAddGlobalLinkFile(const AFile: string);
begin
  TIWBSGlobal.FExclusiveWriteLinkFiles.BeginWrite;
  try
    if FgIWBSLinkFiles = nil then
      FgIWBSLinkFiles := TStringList.Create;
    if FgIWBSLinkFiles.IndexOf(AFile) = -1 then
      FgIWBSLinkFiles.Add(AFile);
  finally
    TIWBSGlobal.FExclusiveWriteLinkFiles.EndWrite;
  end;
end;


class function TIWBSGlobal.IWBSLinkFiles: TStringList;
begin
  TIWBSGlobal.FExclusiveWriteLinkFiles.BeginRead;
  try
    Result:= FgIWBSLinkFiles;
  finally
    TIWBSGlobal.FExclusiveWriteLinkFiles.EndRead;
  end;
end;

class procedure TIWBSGlobal.IWBSRemoveGlobalLinkFile(const AFile: string);
var
  LIndex:Integer;
begin
   TIWBSGlobal.FExclusiveWriteLinkFiles.BeginWrite;
  try
    if FgIWBSLinkFiles = nil then
      FgIWBSLinkFiles := TStringList.Create;
    LIndex:= FgIWBSLinkFiles.IndexOf(AFile);
    if LIndex <> -1 then
      FgIWBSLinkFiles.Delete(LIndex);
  finally
    TIWBSGlobal.FExclusiveWriteLinkFiles.EndWrite;
  end;
end;

initialization
  TIWBSGlobal.Initialize;
  gIWBSRefreshCacheParam := FormatDateTime('yyyymmddhhnnsszzz', now);

finalization
  TIWBSGlobal.Finalize;

end.
