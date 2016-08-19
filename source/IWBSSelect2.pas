unit IWBSSelect2;

interface

uses
  Classes, System.SysUtils, IWBSInput, IWRenderContext, IWHTMLTag, IWXMLTag,
  StrUtils, IWApplication, IW.HTTP.Request, IW.HTTP.Reply, DB, IW.Common.Strings,
  IWBSRestServer, IWBSGlobal, IWBSCommon;

type
  TIWBSSelect2Item = class(TCollectionItem)
  private
    FKey: string;
    FDisplayText: string;
    procedure SetDisplayText(const Value: string);
    procedure SetKey(const Value: string);
  published
    property Key: string read FKey write SetKey;
    property DisplayText: string read FDisplayText write SetDisplayText;
  end;

  TIWBSSelect2Items = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TIWBSSelect2Item;
    procedure SetItem(Index: Integer; const Value: TIWBSSelect2Item);
  public
    property Items[Index: Integer]: TIWBSSelect2Item read GetItem write SetItem;
    function Add: TIWBSSelect2Item;
  end;

  TAsyncSearchEvent = procedure(Sender: TObject; SeachTerm: TStrings) of object;

  TIWBSSelect2 = class(TIWBSSelect)
  private
    FOnAsyncSearch: TAsyncSearchEvent;
        // to update script options when Component options are changed
    procedure UpdateOptions;
        // this event we return a json with the options that the Select2 request
    procedure DoOnAsyncSearch(aApplication: TIWApplication; aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
    procedure SetOnAsyncSearch(const Value: TAsyncSearchEvent);
  protected
    procedure OnItemsChange(ASender : TObject); override;
    procedure InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag); override;
    procedure InternalRenderStyle(AStyle: TStringList); override;
    procedure InternalRenderScript(AContext: TIWCompContext; const AHTMLName: string; AScript: TStringList); override;
    procedure InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RenderAsync(AContext: TIWCompContext): TIWXMLTag; override;
    function RenderHTML(AContext: TIWCompContext): TIWHTMLTag; override;
  published
    property ScriptInsideTag default False;
    property OnAsyncSearch: TAsyncSearchEvent read FOnAsyncSearch write SetOnAsyncSearch;
  end;

implementation
   uses IWBSFluidForm;


{ TIWBSSelect2 }

constructor TIWBSSelect2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ScriptInsideTag := False;
  UpdateOptions;
end;

destructor TIWBSSelect2.Destroy;
begin
  inherited;
end;

procedure TIWBSSelect2.DoOnAsyncSearch(aApplication: TIWApplication; aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
var
  data: string;
  line: string;
  i: Integer;
begin
  if Assigned(FOnAsyncSearch) then
    FOnAsyncSearch(Self, aParams);

  data := '[';
   for i := 0 to Items.Count - 1 do
      begin
        line := '{"id":"' +  InttoStr(i) + '","text":"' +  Items.Names[I]  + '","value":"' + Items.ValueFromIndex[i]  + '"}';
        if I > 0 then
          data := Data + ',';
        data := data + line;
      end;
  data := data + ']';

  aReply.WriteString('{"items": ' + data + '}');
end;

procedure TIWBSSelect2.InternalRenderAsync(const AHTMLName: string;
  AApplication: TIWApplication);
var
  LSelectedIdx: string;
  i: integer;
begin
  inherited;
 {   if (FText <> FOldText) then begin
    LSelectedIdx := '';
    if MultiSelect then
      begin
        for i := 0 to Length(FItemsSelected)-1 do
          if FItemsSelected[i] then begin
            if LSelectedIdx <> '' then
              LSelectedIdx := LSelectedIdx + ',';
            LSelectedIdx := LSelectedIdx + IntToStr(i);
          end;
      end
    else if FItemIndex >= 0 then
      LSelectedIdx := IntToStr(FItemIndex);
    IWBSExecuteAsyncJScript(AApplication, '$("#'+AHTMLName+'").val(['+LSelectedIdx+']);', False, True);
    FOldText := FText;
  end; }

end;

procedure TIWBSSelect2.InternalRenderHTML(const AHTMLName: string;
  AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag);
begin
  inherited;
  AHTMLTag.AddStringParam('style', ActiveStyle);
end;

procedure TIWBSSelect2.InternalRenderScript(AContext: TIWCompContext;
  const AHTMLName: string; AScript: TStringList);
begin
  inherited;
  AScript.Add('$(''#{%htmlname%}'').select2({%options%});');
end;

procedure TIWBSSelect2.InternalRenderStyle(AStyle: TStringList);
begin
  inherited;
  if ParentContainer is TIWBSFluidForm then
    AStyle.Values['width']:=  InttoStr(Width) + 'px';
end;

(*function TIWBSSelect2.IsScriptStored: Boolean;
begin
  { TODO 1 -oDELCIO -cIMPROVEMENT : MOVE TO InternalRenderScripts }
  Result := Script.Text <> '$(''#{%htmlname%}'').select2({%options%});';
end; *)

procedure TIWBSSelect2.OnItemsChange(ASender: TObject);
begin
  inherited;
  UpdateOptions;
end;

function TIWBSSelect2.RenderAsync(AContext: TIWCompContext): TIWXMLTag;
begin
  UpdateOptions;
  Result := inherited RenderAsync(AContext);
end;

function TIWBSSelect2.RenderHTML(AContext: TIWCompContext): TIWHTMLTag;
begin
  UpdateOptions;
  Result := inherited RenderHTML(AContext);
end;



procedure TIWBSSelect2.SetOnAsyncSearch(const Value: TAsyncSearchEvent);
begin
  FOnAsyncSearch := Value;
end;


procedure TIWBSSelect2.UpdateOptions;
var
  OptTxt: TStrings;
begin
  OptTxt := TStringList.Create;
  try
    OptTxt.NameValueSeparator  := ':';
    OptTxt.Delimiter       := ',';
    OptTxt.QuoteChar       := ' ';
    OptTxt.StrictDelimiter := True;

    OptTxt.Values['ajax'] := '{url: "{%dataurl%}",'
                          + 'delay: 200,'
                          + 'dataType: "json",'
                          + 'processResults: function (data) { '
                                + 'return { results: data.items '
                          + '};}}';

    OptTxt.Values['tags']              := 'true';
    OptTxt.Values['placeholder']       := '"Selecione Uma Opção"';
    OptTxt.Values['allowclear']        := 'true';
    OptTxt.Values['width']             := '"style"'; //To Work with IWBSFluidForm
    ScriptParams.Values['options']     := '{' + OptTxt.DelimitedText + '}';

    if CustomRestEvents.Count = 0 then
      CustomRestEvents.Add;
    CustomRestEvents[0].EventName       := 'dataurl';
    CustomRestEvents[0].OnRestEvent     := DoOnAsyncSearch;
  finally
    OptTxt.Free;
  end;
end;


{ TIWBSSelect2Item }

procedure TIWBSSelect2Item.SetDisplayText(const Value: string);
begin
  FDisplayText := Value
end;

procedure TIWBSSelect2Item.SetKey(const Value: string);
begin
  FKey := Value;
end;

{ TIWBSSelect2Items }

function TIWBSSelect2Items.Add: TIWBSSelect2Item;
begin
  Result :=    TIWBSSelect2Item(inherited Add);
end;

function TIWBSSelect2Items.GetItem(Index: Integer): TIWBSSelect2Item;
begin
  Result := TIWBSSelect2Item(inherited GetItem(Index));
end;

procedure TIWBSSelect2Items.SetItem(Index: Integer; const Value: TIWBSSelect2Item);
begin
  inherited SetItem(Index, Value);
end;

initialization
  //Enable CSS and JS for Select2 Plugin
  if DebugHook <> 0 then
    begin
      TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/select2/css/select2.css');
      TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/select2/js/select2.full.js');
    end
  else
    begin
      TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/select2/css/select2.min.css');
      TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/select2/js/select2.full.min.js');
    end;
  // this enable the rest event server
  IWBSRegisterRestServerHandler;
end.

