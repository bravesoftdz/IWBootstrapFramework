unit IWBSCommon;

interface

uses Classes, SysUtils, StrUtils, Forms,
     IWApplication, IWRenderContext, IWControl, IWHTML40Interfaces, IWBaseHTMLInterfaces, IWTypes,
     IWBaseInterfaces, IWHTMLTag, IWBaseRenderContext,
     IWBSJsonDataObjects, IWBSCustomEvents, IW.Common.RenderStream;

const
  EOL = #13#10;

type
  TIWBSTextAlignment = (bstaDefault, bstaLeft, bstaCenter, bstaRight, bstaJustify, bstaNowrap);
  TIWBSTextCase = (bstcDefault, bstcLowercase, bstcUppercase, bstcCapitalize);
  TIWBSSize = (bsszDefault, bsszLg, bsszMd, bsszSm, bsszXs);
  TIWBSRelativeSize = (bsrzDefault, bsrzLg, bsrzSm);
  TIWBSResizeDirection = (bsrdDefault, bsrdNone, bsrdBoth, bsrdVertical, bsrdHorizontal);

const
  aIWBSRelativeSize: array[bsrzDefault..bsrzSm] of string = ('', 'lg', 'sm');
  aIWBSSize: array[bsszDefault..bsszXs] of string = ('', 'lg', 'md', 'sm', 'xs');
  aIWBSTextAlignment: array[bstaDefault..bstaNowrap] of string = ('', 'text-left', 'text-center', 'text-right', 'text-justify', 'text-nowrap');
  aIWBSTextCase: array[bstcDefault..bstcCapitalize] of string = ('', 'text-lowercase', 'text-uppercase', 'text-capitalize');
  aIWBSResizeDirection: array[bsrdDefault..bsrdHorizontal] of string = ('', 'none', 'both', 'vertical', 'horizontal');

type

  TIWBSScriptParams = class(TStringList)
  private
    function GetJson(const Name: string): TJsonObject;
    procedure SetJson(const Name: string; const Value: TJsonObject);
  public
    constructor Create;
    property Json[const Name: string]: TJsonObject read GetJson write SetJson;
  end;


  TIWBSGridOptions = class(TPersistent)
  private
    FGridXSOffset: integer;
    FGridXSSpan: integer;
    FGridSMOffset: integer;
    FGridSMSpan: integer;
    FGridMDOffset: integer;
    FGridMDSpan: integer;
    FGridLGOffset: integer;
    FGridLGSpan: integer;
  public
    constructor Create;
    function GetClassString: string;
    procedure Assign(Source: TPersistent); override;
    class function GetGridClassString(AGridXSOffset, AGridSMOffset, AGridMDOffset, AGridLGOffset, AGridXSSpan, AGridSMSpan, AGridMDSpan, AGridLGSpan: integer): string;
  published
    property GridXSOffset: integer read FGridXSOffset write FGridXSOffset default 0;
    property GridXSSpan: integer read FGridXSSpan write FGridXSSpan default 0;
    property GridSMOffset: integer read FGridSMOffset write FGridSMOffset default 0;
    property GridSMSpan: integer read FGridSMSpan write FGridSMSpan default 0;
    property GridMDOffset: integer read FGridMDOffset write FGridMDOffset default 0;
    property GridMDSpan: integer read FGridMDSpan write FGridMDSpan default 0;
    property GridLGOffset: integer read FGridLGOffset write FGridLGOffset default 0;
    property GridLGSpan: integer read FGridLGSpan write FGridLGSpan default 0;
  end;

  IIWBSComponent = interface(IIWHTML40Control)
    ['{12925CB3-58EC-4B56-B032-478892548906}']
    procedure AsyncRemoveControl;
    procedure AsyncRefreshControl;
    procedure InternalRenderScript(AContext: TIWCompContext; const AHTMLName: string; AScript: TStringList);
    procedure InternalRenderStyle(AStyle: TStringList);
    function HTMLControlImplementation: TIWHTMLControlImplementation;
    function JQSelector: string;
    function ParentContainer: IIWBaseContainer;
    procedure ResetAsyncRefreshControl;
    procedure SetFocus;

    function GetCustomAsyncEvents: TIWBSCustomAsyncEvents;
    procedure SetCustomAsyncEvents(const Value: TIWBSCustomAsyncEvents);
    function IsStoredCustomAsyncEvents: Boolean;
    function GetCustomRestEvents: TIWBSCustomRestEvents;
    procedure SetCustomRestEvents(const Value: TIWBSCustomRestEvents);
    function IsStoredCustomRestEvents: Boolean;

    function GetAfterRender: TNotifyEvent;
    procedure SetAfterRender(const Value: TNotifyEvent);

    function GetScript: TStringList;
    procedure SetScript(const AValue: TStringList);
    function GetScriptInsideTag: boolean;
    procedure SetScriptInsideTag(const AValue: boolean);
    function GetScriptParams: TIWBSScriptParams;
    procedure SetScriptParams(const AValue: TIWBSScriptParams);
    function GetStyle: TStringList;
    procedure SetStyle(const AValue: TStringList);
    function get_Visible: Boolean;
    procedure set_Visible(Value: Boolean);

    property Cursor: TIWCursor read get_WebCursor write set_WebCursor;
    property CustomAsyncEvents: TIWBSCustomAsyncEvents read GetCustomAsyncEvents write SetCustomAsyncEvents;
    property CustomRestEvents: TIWBSCustomRestEvents read GetCustomRestEvents write SetCustomRestEvents;
    property Script: TStringList read GetScript write SetScript;
    property ScriptInsideTag: boolean read GetScriptInsideTag write SetScriptInsideTag;
    property ScriptParams: TIWBSScriptParams read GetScriptParams write SetScriptParams;
    property Style: TStringList read GetStyle write SetStyle;
    property Visible: boolean read get_Visible write set_Visible;

    property OnAfterRender: TNotifyEvent read GetAfterRender write SetAfterRender;
  end;

  IIWBSContainer = interface(IIWBaseContainer)
    ['{819FB21E-8204-450F-8778-3DEB56FDB062}']
    function InitContainerContext(AWebApplication: TIWApplication): TIWContainerContext;
    procedure InternalBeforeRenderControls(var aRenderStream: TIWRenderStream);
    procedure InternalAfterRenderControls(var aRenderStream: TIWRenderStream);
    function ParentContainer: IIWBaseContainer;
    function RegionDiv: TIWHTMLTag;
    function RenderHTML(AContext: TIWCompContext): TIWHTMLTag;

    function get_ContainerContext: TIWContainerContext;
    procedure set_ContainerContext(const AContainerContext: TIWContainerContext);
    function get_HTMLName: String;

    property ContainerContext: TIWContainerContext read get_ContainerContext write set_ContainerContext;
    property HTMLName: string read get_HTMLName;
  end;

  TIWBSCommon = class
  public
    class procedure AddCssClass(var ACss: string; const AClass: string);
    class procedure AsyncRemoveControl(const AHTMLName: string);
    class procedure DoAfterRender(AControl: TComponent);
    class procedure RenderAsync(const AHTMLName: string; AControl: IIWBSComponent; AContext: TIWCompContext);
    class function RenderHTMLTag(AControl: IIWBSComponent; AContext: TIWCompContext): string;
    class function RenderStyle(AComponent: IIWBSComponent): string;
    class function ReplaceParams(AComponent: IIWBSComponent; const AScript: string; AFrom: integer = 1): string;
    class procedure SetNotVisible(AParams: TStrings);
    class procedure ValidateParamName(const AName: string);
    class procedure ValidateTagName(const AName: string);
  end;

procedure SetAsyncDisabled(AApplication: TIWApplication; const HTMLName: string; Value: boolean; var OldValue: boolean);
procedure SetAsyncReadOnly(AApplication: TIWApplication; const HTMLName: string; Value: boolean; var OldValue: boolean);
procedure SetAsyncVisible(AApplication: TIWApplication; const HTMLName: string; Value: boolean; var OldValue: boolean);
procedure SetAsyncClass(AApplication: TIWApplication; const HTMLName: string; const Value: string; var OldValue: string);
procedure SetAsyncStyle(AApplication: TIWApplication; const HTMLName: string; const Value: string; var OldValue: string);
procedure SetAsyncChecked(AApplication: TIWApplication; const HTMLName: string; const Value: boolean; var OldValue: boolean);
procedure SetAsyncText(AApplication: TIWApplication; const HTMLName: string; const Value: string; var OldValue: string);
procedure SetAsyncHtml(AApplication: TIWApplication; const HTMLName: string; const Value: string; var OldValue: string);

implementation

uses IW.Common.System, IWBaseHTMLControl, IWForm,
     IWBSUtils, IWBSCustomControl, IWBSRegionCommon, IWBSGlobal;

{$region 'TIWBSGridOptions'}
constructor TIWBSGridOptions.Create;
begin
  FGridXSOffset := 0;
  FGridXSSpan   := 0;
  FGridSMOffset := 0;
  FGridSMSpan   := 0;
  FGridMDOffset := 0;
  FGridMDSpan   := 0;
  FGridLGOffset := 0;
  FGridLGSpan   := 0;
end;

class function TIWBSGridOptions.GetGridClassString(AGridXSOffset, AGridSMOffset, AGridMDOffset, AGridLGOffset, AGridXSSpan, AGridSMSpan, AGridMDSpan, AGridLGSpan: integer): string;
  procedure AddCssValue(var s: string; const Value: string);
  begin
    if s <> '' then
      s := s + ' ';
    s := s + Value;
  end;
begin
  Result := '';
  if AGridXSOffset > 0 then
    AddCssValue(Result, 'col-xs-offset-'+IntToStr(AGridXSOffset));
  if AGridSMOffset > 0 then
    AddCssValue(Result, 'col-sm-offset-'+IntToStr(AGridSMOffset));
  if AGridMDOffset > 0 then
    AddCssValue(Result, 'col-md-offset-'+IntToStr(AGridMDOffset));
  if AGridLGOffset > 0 then
    AddCssValue(Result, 'col-lg-offset-'+IntToStr(AGridLGOffset));

  if (AGridXSSpan > 0) then
    AddCssValue(Result, 'col-xs-'+IntToStr(AGridXSSpan));
  if (AGridSMSpan > 0) then
    AddCssValue(Result, 'col-sm-'+IntToStr(AGridSMSpan));
  if (AGridMDSpan > 0) then
    AddCssValue(Result, 'col-md-'+IntToStr(AGridMDSpan));
  if (AGridLGSpan > 0) then
    AddCssValue(Result, 'col-lg-'+IntToStr(AGridLGSpan));
end;

function TIWBSGridOptions.GetClassString: string;
begin
  Result := GetGridClassString(FGridXSOffset, FGridSMOffset, FGridMDOffset, FGridLGOffset, FGridXSSpan, FGridSMSpan, FGridMDSpan, FGridLGSpan);
end;

procedure TIWBSGridOptions.Assign(Source: TPersistent);
begin
  if Source is TIWBSGridOptions then
    begin
      GridXSOffset := TIWBSGridOptions(Source).GridXSOffset;
      GridXSSpan := TIWBSGridOptions(Source).GridXSSpan;
      GridSMOffset := TIWBSGridOptions(Source).GridSMOffset;
      GridSMSpan := TIWBSGridOptions(Source).GridSMSpan;
      GridMDOffset := TIWBSGridOptions(Source).GridMDOffset;
      GridMDSpan := TIWBSGridOptions(Source).GridMDSpan;
      GridLGOffset := TIWBSGridOptions(Source).GridLGOffset;
      GridLGSpan := TIWBSGridOptions(Source).GridLGSpan;
    end
  else
    inherited;
end;
{$endregion}

{$region 'AsyncRender functions'}
procedure SetAsyncDisabled(AApplication: TIWApplication; const HTMLName: string; Value: boolean; var OldValue: boolean);
begin
  if OldValue <> Value then begin
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").prop("disabled",'+iif(Value,'true','false')+');', False, True);
    OldValue := Value;
  end;
end;

procedure SetAsyncReadOnly(AApplication: TIWApplication; const HTMLName: string; Value: boolean; var OldValue: boolean);
begin
  if OldValue <> Value then begin
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").prop("readonly",'+iif(Value,'true','false')+');', False, True);
    OldValue := Value;
  end;
end;

procedure SetAsyncVisible(AApplication: TIWApplication; const HTMLName: string; Value: boolean; var OldValue: boolean);
begin
  if OldValue <> Value then begin
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").css("visibility","'+iif(Value,'','hidden')+'");', False, True);
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").css("display","'+iif(Value,'','none')+'");', False, True);
    OldValue := Value;
  end;
end;

procedure SetAsyncText(AApplication: TIWApplication; const HTMLName: string; const Value: string; var OldValue: string);
begin
  if OldValue <> Value then begin
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").val("'+TIWBaseHTMLControl.TextToJSStringLiteral(Value)+'");', False, True);
    OldValue := Value;
  end;
end;

procedure SetAsyncHtml(AApplication: TIWApplication; const HTMLName: string; const Value: string; var OldValue: string);
begin
  if OldValue <> Value then begin
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").html("'+IWBSTextToJsParamText(Value)+'");', True, True);
    OldValue := Value;
  end;
end;

procedure SetAsyncClass(AApplication: TIWApplication; const HTMLName: string; const Value: string; var OldValue: string);
begin
  if OldValue <> Value then begin
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").removeClass().addClass("'+Value+'");', False, True);
    OldValue := Value;
  end;
end;

procedure SetAsyncStyle(AApplication: TIWApplication; const HTMLName: string; const Value: string; var OldValue: string);
begin
  if OldValue <> Value then begin
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").prop("style","'+Value+'");', False, True);
    OldValue := Value;
  end;
end;

procedure SetAsyncChecked(AApplication: TIWApplication; const HTMLName: string; const Value: boolean; var OldValue: boolean);
begin
  if OldValue <> Value then begin
    IWBSExecuteAsyncJScript(AApplication,'$("#'+HTMLName+'").prop("checked",'+iif(Value,'true','false')+');', False, True);
    OldValue := Value;
  end;
end;
{$endregion}

{$region 'TIWBSCommon'}
class procedure TIWBSCommon.AddCssClass(var ACss: string; const AClass: string);
begin
  if ACss <> '' then
    ACss := ACss+' ';
  ACss := ACss+AClass;
end;

class procedure TIWBSCommon.AsyncRemoveControl(const AHTMLName: string);
begin
  IWBSExecuteAsyncJScript('AsyncDestroyControl("'+AHTMLName+'");', False, True);
end;

class procedure TIWBSCommon.DoAfterRender(AControl: TComponent);
var
  i: integer;
  LComponent: IIWBSComponent;
  LContainer: IIWBaseContainer;
begin
  AControl.GetInterface(IIWBSComponent, LComponent);
  if LComponent <> nil then begin
    if Assigned(LComponent.OnAfterRender) then
      LComponent.OnAfterRender(LComponent.InterfaceInstance);

    if Assigned(gIWBSOnAfterRender) then
      gIWBSOnAfterRender(LComponent.InterfaceInstance);
  end;

  if AControl is TFrame then
    for i := 0 to AControl.ComponentCount-1 do
      DoAfterRender(AControl.Components[i])
  else
    begin
      AControl.GetInterface(IIWBaseContainer, LContainer);
      if LContainer <> nil then
        for i := 0 to LContainer.IWComponentsCount-1 do
          DoAfterRender(LContainer.Component[i])
    end;
end;

class procedure TIWBSCommon.RenderAsync(const AHTMLName: string; AControl: IIWBSComponent; AContext: TIWCompContext);
  function ParentTreeVisibility(AControl: TComponent): boolean;
  var
    LContainer: IIWInvisibleControlRenderer;
    LControl: IIWBaseControl;
  begin
    Result := True;
    LControl := BaseControlInterface(AControl);
    if LControl <> nil then begin
      if not LControl.Visible then begin
        GetIWInterface(LControl.ParentContainer.InterfaceInstance, IIWInvisibleControlRenderer, LContainer);
        if not LContainer.RenderInvisibleControls then
          Result := False;
      end;
      if Result and (LControl.ParentContainer <> nil) then
        Result := ParentTreeVisibility(LControl.ParentContainer.InterfaceInstance);
    end;
  end;
var
  LParentContainer: IIWBaseHTMLComponent;
  LBaseContainer: IIWBaseContainer;
  LParentSl: string;
  LHtmlTag: string;
begin

  // if not visible and parent.RenderInvisibleControls is false, do not render
  if not ParentTreeVisibility(AControl.InterfaceInstance) then
    Exit;

  // check if component context is assigned
  if AContext = nil then
    raise Exception.Create('Component context not assigned');

  // if baseContainer, DoRender;
  GetIWInterface(AControl.InterfaceInstance,IIWBaseContainer,LBaseContainer);
  if LBaseContainer <> nil then
    LBaseContainer.DoRender;

  // get parent container
  if AControl.ParentContainer.InterfaceInstance is TIWForm then
    LParentSl := 'body'
  else
    begin
      LParentContainer := BaseHTMLComponentInterface(AControl.ParentContainer.InterfaceInstance);
      if LParentContainer <> nil then
        LParentSl := '#'+LParentContainer.HTMLName
      else
        Exit;
    end;

  LHtmlTag := RenderHtmlTag(AControl, AContext);

  // the creation of the controls is executed as first script in the callback response, so further scripts in callback could reference them
  IWBSExecuteAsyncJScript(AContext.WebApplication, 'AsyncRenderControl("'+AHtmlName+'", "'+LParentSl+'", "'+IWBSTextToJsParamText(LHtmlTag)+'");', True, True);

  DoAfterRender(AControl.InterfaceInstance);
end;

class function TIWBSCommon.RenderHTMLTag(AControl: IIWBSComponent; AContext: TIWCompContext): string;
var
  LContainer: IIWBSContainer;
  LBuffer: TIWRenderStream;
  LTag: TIWHTMLTag;
begin
  LTag := AControl.RenderHTML(AContext);
  try
    if not BaseControlInterface(AControl.InterfaceInstance).Visible then
      TIWBSCommon.SetNotVisible(LTag.Params);

    // render child components
    AControl.InterfaceInstance.GetInterface(IIWBSContainer, LContainer);
    if LContainer <> nil then
      TIWBSRegionCommon.RenderComponents(LContainer, LContainer.InitContainerContext(AContext.WebApplication), AContext.PageContext);

    LBuffer := TIWRenderStream.Create(True, True);
    try
      LTag.Render(LBuffer);
      Result := LBuffer.AsString;
    finally
      LBuffer.Free;
    end;
  finally
    LTag.Free;
  end;

  BaseHTMLComponentInterface(AControl.InterfaceInstance).DoHTMLTag(LTag);
end;

class function TIWBSCommon.RenderStyle(AComponent: IIWBSComponent): string;
var
  xStyle: TStringList;
  i: integer;
begin
  Result := '';

  xStyle := TStringList.Create;
  try
    // assign user style
    xStyle.Assign(AComponent.Style);

    // z-index
    if AComponent.ZIndex <> 0 then
      xStyle.Values['z-index'] := IntToStr(AComponent.Zindex);

    // render cursor
    if AComponent.Cursor <> crAuto then
      xStyle.Values['cursor'] := Copy(TIWCustomControl.RenderCursorStyle(AComponent.Cursor), 9, MaxInt);

    AComponent.InternalRenderStyle(xStyle);

    for i := 0 to xStyle.Count-1 do begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + xStyle[i];
    end;
  finally
    xStyle.Free;
  end;
end;

class function TIWBSCommon.ReplaceParams(AComponent: IIWBSComponent; const AScript: string; AFrom: integer = 1): string;
var
  LF, LT, i: integer;
  LParam, LParNm: string;
  LFound: boolean;
  LCompo: Tcomponent;
  LIComp: IIWBSComponent;
begin
  Result := AScript;

  LF := PosEx('{%', Result, AFrom);
  if LF > 0 then begin
    LFound := False;

    LT := PosEx('%}', Result, LF);
    if LT > LF then
      LParam := Copy(Result, LF, LT-LF+2);
      LParNm := Copy(Result, LF+2, LT-LF-2);

      i := AComponent.ScriptParams.IndexOfName(LParNm);
      if i >= 0 then begin
        Result := ReplaceStr(Result, LParam, AComponent.ScriptParams.ValueFromIndex[i]);
        LFound := True;
      end;

      i := AComponent.ScriptParams.IndexOf(LParNm);
      if i >= 0 then begin
        if AComponent.ScriptParams.Objects[i] is TJsonObject then
          Result := ReplaceText(Result, LParam, TJsonObject(AComponent.ScriptParams.Objects[i]).ToJSON)
        else
          Result := ReplaceText(Result, LParam, '');
      end;

      if not LFound and AnsiSameText('htmlname', LParNm) then begin
        Result := ReplaceStr(Result, LParam, AComponent.HTMLName);
        LFound := True;
      end;

      if not LFound and (AComponent.InterfaceInstance.Owner <> nil) then begin
        LCompo := AComponent.InterfaceInstance.Owner.FindComponent(LParNm);
        if (LCompo <> nil) then begin
          LCompo.GetInterface(IIWBSComponent, LIComp);
          if LIComp <> nil then begin
            Result := ReplaceStr(Result, LParam, LIComp.HTMLName);
            LFound := True;
          end;
        end;
      end;

      if LFound then
        Result := ReplaceParams(AComponent, Result)
      else
        Result := ReplaceParams(AComponent, Result, LF+1);
  end;
end;

class procedure TIWBSCommon.ValidateParamName(const AName: string);
var
  i: integer;
begin
  for i := 1 to Length(AName) do
    if not CharInSet(AName[i], ['-','.','0'..'9','A'..'Z','a'..'z']) then
      raise Exception.Create('Invalid character in param name '+AName);
end;

class procedure TIWBSCommon.ValidateTagName(const AName: string);
var
  i: integer;
begin
  if AName = '' then
    Exception.Create('Tag name could not be empty');
  for i := 1 to Length(AName) do
    if ((i = 1) and not CharInSet(AName[i], ['A'..'Z','a'..'z'])) or
       ((i > 1) and not CharInSet(AName[i], ['0'..'9','A'..'Z','a'..'z'])) then
      raise Exception.Create('Invalid character in tag name '+AName);
end;

class procedure TIWBSCommon.SetNotVisible(AParams: TStrings);
var
  LStyle: string;
begin
  LStyle := AParams.Values['style'];
  LStyle := Trim(LStyle);
  if (LStyle <> '') and not AnsiEndsStr(';', LStyle) then
    LStyle := LStyle+';';
  if not AnsiContainsStr(LStyle, 'visibility:') then
    LStyle := LStyle +  'visibility: hidden;';
  if not AnsiContainsStr(LStyle, 'display:') then
    LStyle := LStyle +  'display: none;';
  AParams.Values['style'] := LStyle;
end;
{$endregion}

{$region 'TIWBSScriptParams'}
constructor TIWBSScriptParams.Create;
begin
  inherited;
  Duplicates := dupError;
  OwnsObjects := True;
end;

function TIWBSScriptParams.GetJson(const Name: string): TJsonObject;
var
  i: integer;
begin
  i := IndexOf(Name);
  if i < 0 then
    begin
      Result := TJsonObject.Create;
      AddObject(Name, Result);
    end
  else if Objects[i] = nil then
    begin
      Result := TJsonObject.Create;
      Objects[i] := Result;
    end
  else
    Result := TJsonObject(Objects[i]);
end;

procedure TIWBSScriptParams.SetJson(const Name: string; const Value: TJsonObject);
begin
  Json[Name].Assign(Value);
end;
{$endregion}

end.
