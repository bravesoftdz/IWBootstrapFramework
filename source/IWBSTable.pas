unit IWBSTable;

interface

uses System.Classes, System.SysUtils, System.Math,
  IWBSControls, IWRenderContext, IWHTMLTag,
  IWXMLTag, StrUtils, IWApplication, IW.HTTP.Request, IW.HTTP.Reply, DB,
  IW.Common.Strings, IWBSRestServer, IWBSGlobal;

type

  TBsTblSortOrder     = (bsgSortAsc, bsgSortDesc);
  TBsTblPagPosition   = (bsgPagTop, bsgPagBottom, bsgPagBoth);
  TBsColDataAlign     = (bscAlLeft, bscAlRight, bscAlCenter);
  TBsColDataVertAlign = (bscAlVerTop, bscAlVerBottom, bscAlVerMiddle);

  TIWBSTableColumn = class(TCollectionItem)
  private
    FFieldName: string;
    FTitle: string;
    FCSSclass: string;
    FDataAlign: TBsColDataAlign;
    FHeaderAlign: TBsColDataAlign;
    FFooterAlign: TBsColDataAlign;
    FDataVertAlign: TBsColDataVertAlign;
    FWidth: string;
    FSortable: Boolean;
    procedure SetFieldName(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetCSSclass(const Value: string);
    procedure SetDataAlign(const Value: TBsColDataAlign);
    procedure SetHeaderAlign(const Value: TBsColDataAlign);
    procedure SetFooterAlign(const Value: TBsColDataAlign);
    procedure SetDataVertAlign(const Value: TBsColDataVertAlign);
    procedure SetWidth(const Value: string);
    procedure SetSortable(const Value: Boolean);
    function IsCssClassStored: Boolean;
    function IsWidthStored: Boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property FieldName: string read FFieldName write SetFieldName;
    property Title: string read FTitle write SetTitle;
    property CSSclass:string read FCSSclass write SetCSSclass stored IsCssClassStored;
    property DataAlign:TBsColDataAlign read FDataAlign write SetDataAlign default bscAlLeft;
    property DataVertAlign:TBsColDataVertAlign read FDataVertAlign write SetDataVertAlign default bscAlVerMiddle;
    property HeaderAlign: TBsColDataAlign read FHeaderAlign write SetHeaderAlign default bscAlLeft;
    property FooterAlign: TBsColDataAlign read FFooterAlign write SetFooterAlign default bscAlLeft;
    property Width: string read FWidth write SetWidth stored IsWidthStored;
    property Sortable: Boolean read FSortable write SetSortable default True;
  end;

  TIWBSTable = class(TIWBSText)
  private
    FColumns: TOwnedCollection;
    FPagination: Boolean;
    FMobileResponsive: Boolean;
    FFormatData: TFormatSettings;
    FSortColumn: string;
    FSortOrder: TBsTblSortOrder;
    FShowHeader: Boolean;
    FShowFooter: Boolean;
    FShowRefresh: Boolean;
    FPaginationPosition: TBsTblPagPosition;
    FclickToSelect: Boolean;
    FSingleSelect: Boolean;
    procedure SetTagType(const Value: string);
    function IsTagTypeStored: Boolean;
    function IsSortFieldNameStored: Boolean;
    procedure SetColumns(const Value: TOwnedCollection);
    // to update script table options when Component options are changed
    procedure UpdateOptions;
    // if no columns set an datasource is setted, add all dataset columns
    procedure VerifyColumns;
    procedure SetMobileResponsive(const Value: Boolean);
    procedure SetPagination(const Value: Boolean);
    // this event we return a json with the rows that the bootstrap table request
    procedure DbTableCustomRestEvents0RestEvent(aApplication: TIWApplication;
      aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
    function GetColumns: TOwnedCollection;
    function GetTagType: string;
    procedure SetSortColumn(const Value: string);
    procedure SetSortOrder(const Value: TBsTblSortOrder);
    procedure SetShowHeader(const Value: Boolean);
    procedure SetShowFooter(const Value: Boolean);
    procedure SetShowRefresh(const Value: Boolean);
    procedure SetPaginationPosition(const Value: TBsTblPagPosition);
    procedure SetclickToSelect(const Value: Boolean);
    procedure SetSingleSelect(const Value: Boolean);
  protected
     procedure InternalRenderScript(AContext: TIWCompContext; const AHTMLName: string; AScript: TStringList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RenderAsync(AContext: TIWCompContext): TIWXMLTag; override;
    function RenderHTML(AContext: TIWCompContext): TIWHTMLTag; override;
  published
    property TagType: string read GetTagType write SetTagType stored IsTagTypeStored;
    property ScriptInsideTag default False;
    property Columns: TOwnedCollection read GetColumns write SetColumns;
    property Pagination: Boolean read FPagination write SetPagination default True;
    property MobileResponsive: Boolean read FMobileResponsive write SetMobileResponsive default True;
    property SortFieldName:string read FSortColumn write SetSortColumn stored IsSortFieldNameStored;
    property SortOrder: TBsTblSortOrder read FSortOrder write SetSortOrder default bsgSortAsc;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property ShowFooter:Boolean read FShowFooter write SetShowFooter default False;
    property ShowRefresh:Boolean read FShowRefresh write SetShowRefresh default True;
    property PaginationPosition:TBsTblPagPosition read FPaginationPosition write SetPaginationPosition default bsgPagBottom;
    property ClickToSelect:Boolean read FclickToSelect write SetclickToSelect default True;
    property SingleSelect:Boolean read FSingleSelect write SetSingleSelect default True;
  end;

implementation

{ TIWBSTable }

constructor TIWBSTable.Create(AOwner: TComponent);
begin
  inherited;
  inherited TagType         := 'table';
  inherited ScriptInsideTag := False;
  FColumns              := TOwnedCollection.Create(Self, TIWBSTableColumn);
  FPagination           := True;
  FMobileResponsive     := True;
  FFormatData           := TFormatSettings.Create('en-US');
  FSortColumn           := '';
  FSortOrder            := bsgSortAsc;
  FShowHeader           := True;
  FShowFooter           := False;
  FShowRefresh          := True;
  FPaginationPosition   := bsgPagBottom;
  FclickToSelect        := True;
  FSingleSelect         := True;
  UpdateOptions;
end;

procedure TIWBSTable.DbTableCustomRestEvents0RestEvent(aApplication: TIWApplication;
  aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
  function GetFieldForColumn(aColumnIndex: integer): TField;
  begin
    try
    Result := DataSource.DataSet.FieldByName(TIWBSTableColumn(FColumns.Items[aColumnIndex])
      .FieldName);
    except
      Result:=nil;
    end;
  end;

var
  data: string;
  line: string;
  bmrk: TBookmark;
  r, i, f, t: integer;
begin
  If (NOT Assigned(DataSource)) or (not Assigned(DataSource.DataSet)) then
    Exit;
  // here we return the data in json format
  // see format on: http://bootstrap-table.wenzhixin.net.cn/getting-started/#usage-via-javascript
  f := StrToIntDef(aRequest.QueryFields.Values['offset'], 0);
  t := Min(f + StrToIntDef(aRequest.QueryFields.Values['limit'], 10),
    DataSource.DataSet.RecordCount);

  DataSource.DataSet.DisableControls;
  bmrk := DataSource.DataSet.Bookmark;
  try
    data  := '';
    for r := f + 1 to t do
      begin
        DataSource.DataSet.RecNo := r;

        line  := '';
        for i := 0 to FColumns.Count - 1 do
          begin
            if i > 0 then
              line := line + ',';
            if (GetFieldForColumn(i) is TNumericField) then
              line := line + '"field' + IntToStr(i) + '":' +
                FloatToStr(GetFieldForColumn(i).AsFloat, FFormatData)
            else if (DataSource.DataSet.Fields[i] is TStringField) or
              (GetFieldForColumn(i) is TMemoField) then
              line := line + '"field' + IntToStr(i) + '":"' +
                EscapeJsonString(GetFieldForColumn(i).AsString) + '"'
            else if GetFieldForColumn(i) <> nil then
               line := line + '"field' + IntToStr(i) + '":"' +
                GetFieldForColumn(i).AsString + '"'
            else
              line := line + '"field' + IntToStr(i) + '":""';
          end;
        if data <> '' then
          data := data + ',';
        data   := data + '{' + line + '}';
      end;
    aReply.WriteString('{"total": ' + IntToStr(DataSource.DataSet.RecordCount) + ', "rows": [' +
      data + ']}');
  finally
    DataSource.DataSet.GotoBookmark(bmrk);
    DataSource.DataSet.EnableControls;
  end;
end;

destructor TIWBSTable.Destroy;
begin
  FColumns.Free;
  inherited;
end;

function TIWBSTable.GetColumns: TOwnedCollection;
begin
  { if not Assigned(FColumns) then
    FColumns:= TOwnedCollection.Create(Self, TIwSrpTableColum); }
  Result := FColumns;
end;

function TIWBSTable.GetTagType: string;
begin
  Result := inherited TagType;
end;

procedure TIWBSTable.InternalRenderScript(AContext: TIWCompContext;
  const AHTMLName: string; AScript: TStringList);
begin
  inherited;
  UpdateOptions;
  AScript.Add('$(''#{%htmlname%}'').bootstrapTable({%options%});');



end;



function TIWBSTable.IsSortFieldNameStored: Boolean;
begin
  Result := FSortColumn <> '';
end;

function TIWBSTable.IsTagTypeStored: Boolean;
begin
  Result := TagType <> 'table';
end;

function TIWBSTable.RenderAsync(AContext: TIWCompContext): TIWXMLTag;
begin
  if FColumns.Count = 0 then
    UpdateOptions;
  Result := inherited;
end;

function TIWBSTable.RenderHTML(AContext: TIWCompContext): TIWHTMLTag;
begin
  if FColumns.Count = 0 then
    UpdateOptions;
  Result := inherited;
end;

procedure TIWBSTable.VerifyColumns;
var
  J: integer;
begin
  if (FColumns.Count = 0) and (Assigned(DataSource)) then
    begin
      for J := 0 to DataSource.DataSet.FieldCount - 1 do
        begin
          with TIWBSTableColumn(FColumns.Add) do
            begin
              FFieldName := DataSource.DataSet.Fields[J].FieldName;
              Title      := DataSource.DataSet.Fields[J].DisplayName;
            end;
        end;
    end;
end;

procedure TIWBSTable.SetclickToSelect(const Value: Boolean);
begin
  if FclickToSelect <> Value then
    begin
      FclickToSelect := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetColumns(const Value: TOwnedCollection);
begin
  { if not Assigned(FColumns) then
    FColumns:= TOwnedCollection.Create(Self, TIwSrpTableColum); }
  if Value <> FColumns then
    begin
      FColumns.Assign(Value);
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetMobileResponsive(const Value: Boolean);
begin
  if FMobileResponsive <> Value then
    begin
      FMobileResponsive := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetPagination(const Value: Boolean);
begin
  FPagination := Value;
  UpdateOptions;
end;

procedure TIWBSTable.SetPaginationPosition(const Value: TBsTblPagPosition);
begin
  if FPaginationPosition <> Value then
    begin
      FPaginationPosition := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetShowFooter(const Value: Boolean);
begin
  if FShowFooter <> Value then
    begin
      FShowFooter := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetShowHeader(const Value: Boolean);
begin
  if FShowHeader <> Value then
    begin
      FShowHeader := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetShowRefresh(const Value: Boolean);
begin
  if FShowRefresh <> Value then
    begin
      FShowRefresh := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetSingleSelect(const Value: Boolean);
begin
  if FSingleSelect <> Value then
    begin
      FSingleSelect := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetSortColumn(const Value: string);
begin
  if FSortColumn <> Value then
    begin
      FSortColumn := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetSortOrder(const Value: TBsTblSortOrder);
begin
  if FSortOrder <> Value then
    begin
      FSortOrder := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSTable.SetTagType(const Value: string);
begin
  inherited TagType := Value;
  UpdateOptions;
end;

procedure TIWBSTable.UpdateOptions;
var
  OptColumns: string;
  OptTxt: TStrings;
  J: integer;
begin
  VerifyColumns;

  OptColumns := '[';
  for J      := 0 to FColumns.Count - 1 do
    begin
      with TIWBSTableColumn(FColumns.Items[J]) do
        begin
          if J > 0 then
            OptColumns := OptColumns + ',';
          OptColumns := OptColumns + '{"field":' + '"field' + IntToStr(J) + '","title":"' + FTitle;
          if FCSSclass <> '' then
            OptColumns := OptColumns + '","class":"' + FCSSclass;
          case FDataAlign of
            bscAlLeft:
              OptColumns := OptColumns + '","align":"left';
            bscAlRight:
              OptColumns := OptColumns + '","align":"right';
            bscAlCenter:
              OptColumns := OptColumns + '","align":"center';
          end;
          case FDataVertAlign of
            bscAlVerTop:
              OptColumns := OptColumns + '","valign":"top';
            bscAlVerBottom:
              OptColumns := OptColumns + '","valign":"bottom';
            bscAlVerMiddle:
              OptColumns := OptColumns + '","valign":"middle';
          end;
          case FHeaderAlign of
            bscAlLeft:
              OptColumns := OptColumns + '","halign":"left';
            bscAlRight:
              OptColumns := OptColumns + '","halign":"right';
            bscAlCenter:
              OptColumns := OptColumns + '","halign":"center';
          end;
          case FFooterAlign of
            bscAlLeft:
              OptColumns := OptColumns + '","falign":"left';
            bscAlRight:
              OptColumns := OptColumns + '","falign":"right';
            bscAlCenter:
              OptColumns := OptColumns + '","falign":"center';
          end;
          if (FWidth <> '') then
            OptColumns := OptColumns + '","width":"' + FWidth;
          OptColumns := OptColumns + IfThen(FSortable, '","sortable":"true', '","sortable":"false');
          OptColumns := OptColumns + '"}';
        end;

    end;
  OptColumns := OptColumns + ']';

  OptTxt := TStringList.Create;
  try
    OptTxt.NameValueSeparator := ':';
    OptTxt.Delimiter          := ',';
    OptTxt.QuoteChar          := ' ';
    OptTxt.StrictDelimiter    := True;
    try

    OptTxt.Values['url']              := '"'+ IWBSRegisterRestCallBack(gGetWebApplicationThreadVar, HTMLName +'.dataurl', DbTableCustomRestEvents0RestEvent) + '"';
    except

    end;
    OptTxt.Values['columns']          := OptColumns;
    OptTxt.Values['pagination']       := IfThen(FPagination, 'true', 'false');
    OptTxt.Values['sidePagination']   := '"server"';
    OptTxt.Values['mobileResponsive'] := IfThen(FMobileResponsive, 'true', 'false');
    // OptTxt.Values['paginationVAlign'] := '"top"';
    if FSortColumn <> '' then
      OptTxt.Values['sortName']  := '"' + FSortColumn + '"';
    OptTxt.Values['sortOrder']   := IfThen(FSortOrder = bsgSortAsc, '"asc"', '"desc"');
    OptTxt.Values['showHeader']  := IfThen(FShowHeader, 'true', 'false');
    OptTxt.Values['showFooter']  := IfThen(FShowFooter, 'true', 'false');
    OptTxt.Values['showRefresh'] := IfThen(FShowRefresh, 'true', 'false');
    case FPaginationPosition of
      bsgPagTop:
        OptTxt.Values['paginationVAlign'] := '"top"';
      bsgPagBottom:
        OptTxt.Values['paginationVAlign'] := '"bottom"';
      bsgPagBoth:
        OptTxt.Values['paginationVAlign'] := '"both"';
    end;
    OptTxt.Values['clickToSelect'] := IfThen(FclickToSelect, 'true', 'false');
    OptTxt.Values['singleSelect']  := IfThen(FSingleSelect, 'true', 'false');

    ScriptParams.Values['options'] := '{' + OptTxt.DelimitedText + '}';

  finally
    OptTxt.Free;
  end;
end;

{ TIWBSTableColumn }

constructor TIWBSTableColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCSSclass      := '';
  FDataAlign     := bscAlLeft;
  FDataVertAlign := bscAlVerMiddle;
  FHeaderAlign   := bscAlLeft;
  FFooterAlign   := bscAlLeft;
  FWidth         := '';
  FSortable      := True;
end;

destructor TIWBSTableColumn.Destroy;
begin

  inherited;
end;

function TIWBSTableColumn.IsCssClassStored: Boolean;
begin
  Result := FCSSclass <> '';
end;

function TIWBSTableColumn.IsWidthStored: Boolean;
begin
  Result := FWidth <> '';
end;

procedure TIWBSTableColumn.SetCSSclass(const Value: string);
begin
  if FCSSclass <> Value then
    begin
      FCSSclass := Value;
      // (GetOwner as TIWBSTable).UpdateOptions;
    end;
end;

procedure TIWBSTableColumn.SetDataAlign(const Value: TBsColDataAlign);
begin
  if FDataAlign <> Value then
    begin
      FDataAlign := Value;
      // (GetOwner as TIWBSTable).UpdateOptions;
    end;
end;

procedure TIWBSTableColumn.SetDataVertAlign(const Value: TBsColDataVertAlign);
begin
  if FDataVertAlign <> Value then
    begin
      FDataVertAlign := Value;
      // (GetOwner as TIWBSTable).UpdateOptions;
    end;
end;

procedure TIWBSTableColumn.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

procedure TIWBSTableColumn.SetFooterAlign(const Value: TBsColDataAlign);
begin
  if FFooterAlign <> Value then
    begin
      FFooterAlign := Value;
      // (GetOwner as TIWBSTable).UpdateOptions;
    end;
end;

procedure TIWBSTableColumn.SetHeaderAlign(const Value: TBsColDataAlign);
begin
  if FHeaderAlign <> Value then
    begin
      FHeaderAlign := Value;
      // (GetOwner as TIWBSTable).UpdateOptions;
    end;
end;

procedure TIWBSTableColumn.SetSortable(const Value: Boolean);
begin
  if FSortable <> Value then
    begin
      FSortable := Value;
      // (GetOwner as TIWBSTable).UpdateOptions;
    end;
end;

procedure TIWBSTableColumn.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TIWBSTableColumn.SetWidth(const Value: string);
begin
  if FWidth <> Value then
    begin
      FWidth := Value;
      // (GetOwner as TIWBSTable).UpdateOptions;
    end;
end;

initialization

// Enable CSS and JS for Table Plugin
if DebugHook <> 0 then
  begin
    IWBSAddGlobalLinkFile('/<iwbspath>/bstable/bootstrap-table.css');
    IWBSAddGlobalLinkFile('/<iwbspath>/bstable/bootstrap-table.js');
  end
else
  begin
    IWBSAddGlobalLinkFile('/<iwbspath>/bstable/bootstrap-table.min.css');
    IWBSAddGlobalLinkFile('/<iwbspath>/bstable/bootstrap-table.min.js');
  end;
if gIWBSLibTableMobileResponsive then
  IWBSAddGlobalLinkFile('/<iwbspath>/bstable/bootstrap-table-mobile.js');


// this enable the rest event server
IWBSRegisterRestServerHandler;

end.
