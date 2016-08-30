unit IWBSJQGrid;

interface

uses System.Classes, System.SysUtils, System.Math,
  IWBSControls, IWRenderContext, IWHTMLTag,
  IWXMLTag, StrUtils, IWApplication, IW.HTTP.Request, IW.HTTP.Reply, DB,
  IW.Common.Strings, IWBSRestServer, IWBSGlobal;

type

  TBsJQGSortOrder     = (bsgSortAsc, bsgSortDesc);
  TBsJQGPagPosition   = (bsgPagTop, bsgPagBottom, bsgPagBoth);

  TBsColDataAlign     = (bscAlDefault, bscAlLeft, bscAlRight, bscAlCenter);
 // TBsColDataVertAlign = (bscAlVerTop, bscAlVerBottom, bscAlVerMiddle);
 TbsJQGColInputType = (bsjqgText, bsjqgSelect, bsjqgCustom);

  TIWBSJQGridColumn = class(TCollectionItem)
  private
    FFieldName: string;
    FTitle: string;
    FCSSclass: string;
    FDataAlign: TBsColDataAlign;
   FHeaderAlign: TBsColDataAlign;
   // FFooterAlign: TBsColDataAlign;
   // FDataVertAlign: TBsColDataVertAlign;
    FWidth: string;
    FSortable: Boolean;
    FInputType: TbsJQGColInputType;
    FResizable: Boolean;
    FVisible: Boolean;
    FEditable: Boolean;
    procedure SetFieldName(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetCSSclass(const Value: string);
    procedure SetDataAlign(const Value: TBsColDataAlign);
    procedure SetHeaderAlign(const Value: TBsColDataAlign);
   // procedure SetFooterAlign(const Value: TBsColDataAlign);
   // procedure SetDataVertAlign(const Value: TBsColDataVertAlign);
    procedure SetWidth(const Value: string);
    procedure SetSortable(const Value: Boolean);
    function IsWidthStored: Boolean;
    function IsCssClassStored: Boolean;
    procedure SetInputType(const Value: TbsJQGColInputType);
    procedure SetResizable(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetEditable(const Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    //The Dataset Field for column
    property FieldName: string read FFieldName write SetFieldName;
    //the title of Column
    property Title: string read FTitle write SetTitle;
    //the css class for all cell in this column
    property CSSclass:string read FCSSclass write SetCSSclass stored IsCssClassStored;
    //the alignament data in all cell in this column
    property DataAlign:TBsColDataAlign read FDataAlign write SetDataAlign default bscAlDefault;
  //  property DataVertAlign:TBsColDataVertAlign read FDataVertAlign write SetDataVertAlign default bscAlVerMiddle;
    //the alignament of header tittle
    property HeaderAlign: TBsColDataAlign read FHeaderAlign write SetHeaderAlign default bscAlCenter;
   // property FooterAlign: TBsColDataAlign read FFooterAlign write SetFooterAlign default bscAlLeft;
    //the width of column in px or %
    property Width: string read FWidth write SetWidth stored IsWidthStored;
    //if Column is sortable or not
    property Sortable: Boolean read FSortable write SetSortable default True;
    //if column is editable or not
    property Editable:Boolean read FEditable write SetEditable default True;
    //the input type for column in edit mode. Require Editable = True;
    property InputType: TbsJQGColInputType read FInputType write SetInputType;
    //if the column is resizable in browser
    property Resizable: Boolean read FResizable write SetResizable default True;
    //if the column is visible
    property Visible:Boolean read FVisible write SetVisible default True;
  end;

  TIWBSJQGrid = class(TIWBSText)
  private
    FColumns: TOwnedCollection;
    FPagination: Boolean;
    FMobileResponsive: Boolean;
    FFormatData: TFormatSettings;
    FSortColumn: string;
    FSortOrder: TBsJQGSortOrder;
    FShowHeader: Boolean;
    FShowFooter: Boolean;
    FShowRefresh: Boolean;
    FPaginationPosition: TBsJQGPagPosition;
    FclickToSelect: Boolean;
    FSingleSelect: Boolean;
    data: string;
    FGroupingField: string;
    FGroupSummary: Boolean;
    FGroupCollapse: Boolean;
    FGroupLabel: string;
    FKeyField: string;
    procedure SetTagType(const Value: string);
    function IsTagTypeStored: Boolean;
    function IsSortFieldNameStored: Boolean;
    procedure SetColumns(const Value: TOwnedCollection);
    // to update script JQGrid options when Component options are changed
    procedure UpdateOptions;
    // if no columns set an datasource is setted, add all dataset columns
    procedure VerifyColumns;
    procedure SetMobileResponsive(const Value: Boolean);
    procedure SetPagination(const Value: Boolean);
    // this event we return a json with the rows that the bootstrap JQGrid request
    procedure DbJQGridCustomRestEvents0RestEvent(aApplication: TIWApplication;
      aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
    function GetColumns: TOwnedCollection;
    function GetTagType: string;
    procedure SetSortColumn(const Value: string);
    procedure SetSortOrder(const Value: TBsJQGSortOrder);
    procedure SetShowHeader(const Value: Boolean);
    procedure SetShowFooter(const Value: Boolean);
    procedure SetShowRefresh(const Value: Boolean);
    procedure SetPaginationPosition(const Value: TBsJQGPagPosition);
    procedure SetclickToSelect(const Value: Boolean);
    procedure SetSingleSelect(const Value: Boolean);
    procedure SetGroupingField(const Value: string);
    procedure SetGroupCollapse(const Value: Boolean);
    procedure SetGroupSummary(const Value: Boolean);
    procedure SetGroupLabel(const Value: string);
    procedure DoOnRowSel(aParams:TStringList);
    procedure DoEditRow(aApplication: TIWApplication;
              aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
    procedure SetKeyField(const Value: string);
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
    //Collection of Columns in the grid.
    //To acess Columns.Items[n] properties do a TypeCast eg: TIWBSJQGridColumn(Columns.Items[n]).FieldName
    property Columns: TOwnedCollection read GetColumns write SetColumns;
    //property Pagination: Boolean read FPagination write SetPagination default True;
    //property MobileResponsive: Boolean read FMobileResponsive write SetMobileResponsive default True;
    //property SortFieldName:string read FSortColumn write SetSortColumn stored IsSortFieldNameStored;
   // property SortOrder: TBsJQGSortOrder read FSortOrder write SetSortOrder default bsgSortAsc;
    //property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
   // property ShowFooter:Boolean read FShowFooter write SetShowFooter default False;
    //property ShowRefresh:Boolean read FShowRefresh write SetShowRefresh default True;
   //property PaginationPosition:TBsJQGPagPosition read FPaginationPosition write SetPaginationPosition default bsgPagBottom;
    //property ClickToSelect:Boolean read FclickToSelect write SetclickToSelect default True;
    //property SingleSelect:Boolean read FSingleSelect write SetSingleSelect default True;
    //Field name for Grouping rows
    property GroupingField:string read FGroupingField write SetGroupingField;
    //To show group summary, require GroupingField;
    property GroupSummary:Boolean read FGroupSummary write SetGroupSummary default False;
    //To show group rows collapsed, require GroupingField;
    property GroupCollapse:Boolean read FGroupCollapse write SetGroupCollapse default False;
    //Text to show before group header value
    property GroupLabel:string read FGroupLabel write SetGroupLabel;
    //The key for record navigation and edition
    //!!!!ATENTION!!!!! this field should be the primary key or
    //unique value field in the table, else data can be saved in the wrong records
    property KeyField:string read FKeyField write SetKeyField;
  end;


implementation

{ TIWBSJQGrid }

constructor TIWBSJQGrid.Create(AOwner: TComponent);
begin
  inherited;
  inherited TagType         := 'table';
  inherited ScriptInsideTag := False;
  FColumns              := TOwnedCollection.Create(Self, TIWBSJQGridColumn);
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
  FGroupingField:= '';
  FGroupSummary:= False;
  FGroupCollapse:= False;
  FGroupLabel:= '';
  FKeyField:='';
  UpdateOptions;
end;

procedure TIWBSJQGrid.DbJQGridCustomRestEvents0RestEvent(aApplication: TIWApplication;
  aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
  function GetFieldForColumn(aColumnIndex: integer): TField;
  begin
    try
    Result := DataSource.DataSet.FieldByName(TIWBSJQGridColumn(FColumns.Items[aColumnIndex])
      .FieldName);
    except
      Result:=nil;
    end;
  end;

var

  line: string;
  bmrk: TBookmark;
  r, i, f, t: integer;
  Lcallback: string;
  Rows, Page:Integer;
begin
  Lcallback:= aParams.Values['callback'];
  //f:=0;
  //t:= DataSource.DataSet.RecordCount;
  Rows:= StrToIntDef(aRequest.QueryFields.Values['rows'], 20);
  Page:= StrToIntDef(aRequest.QueryFields.Values['page'], 1);
  f := (Page * Rows) - Rows;
  t := Min(f + Rows, DataSource.DataSet.RecordCount);


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
              line := line + TIWBSJQGridColumn(FColumns.Items[i]).FFieldName + ':"' +
                FloatToStr(GetFieldForColumn(i).AsFloat, FFormatData) + '"'
            else if (DataSource.DataSet.Fields[i] is TStringField) or
              (GetFieldForColumn(i) is TMemoField) then
              line := line + TIWBSJQGridColumn(FColumns.Items[i]).FFieldName + ':"' +
                EscapeJsonString(GetFieldForColumn(i).AsString) + '"'
            else if GetFieldForColumn(i) <> nil then
               line := line + TIWBSJQGridColumn(FColumns.Items[i]).FFieldName + ':"' +
                GetFieldForColumn(i).AsString + '"'
            else
              line := line + TIWBSJQGridColumn(FColumns.Items[i]).FFieldName + ':""';
          end;
        if data <> '' then
          data := data + ',';
        data   := data + '{' + line + '}';
      end;
    aReply.WriteString(Lcallback + '({"records":"' + IntToStr(DataSource.DataSet.RecordCount) + '","page":' + IntToStr(Page) + ',"total":' +  IntToStr(DataSource.DataSet.RecordCount div Rows) + ',"rows": [' +
      data + ']})');
  finally
    DataSource.DataSet.GotoBookmark(bmrk);
    DataSource.DataSet.EnableControls;
  end;
end;

destructor TIWBSJQGrid.Destroy;
begin

  FColumns.Free;
  inherited;
end;

procedure TIWBSJQGrid.DoEditRow(aApplication: TIWApplication;
  aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
var
  KeyValue:Variant;
  Value:Variant;
  Field:TField;
  I: Integer;
begin
  if aParams.Values['oper'] = 'edit' then
    begin
      if FKeyField = '' then
        raise Exception.Create('No KeyField set. This must be a primarykey or unique field.');
      KeyValue:= aParams.Values[FKeyField];
      if DataSource.DataSet.FieldByName(FKeyField).Value <> KeyValue then
        begin
          if Not DataSource.DataSet.Locate(FKeyField, KeyValue, [loCaseInsensitive]) then
            begin
              gGetWebApplicationThreadVar.ShowMessage('Record not found on the server');
              gGetWebApplicationThreadVar.CallBackResponse.AddJavaScriptToExecute(
                   '$("#' + HTMLName + '").trigger("reloadGrid")'
              );
            end;
        end;

      if DataSource.DataSet.FieldByName(FKeyField).Value = KeyValue then
        begin
          for I := 0 to aParams.Count -1 do
            begin
              if aParams.Names[I] <> 'oper' then
                begin
                  Field:= DataSource.DataSet.FieldByName(aParams.Names[I]);
                  Value:= aParams.Values[aParams.Names[I]];
                  if Field <> nil then
                    begin
                      if Field.Value <> Value then
                        begin
                          if Field.DataSet.State in [dsEdit, dsInsert] then
                            Field.Value:= Value
                          else
                            begin
                              Field.DataSet.Edit;
                              Field.Value:= Value;
                              Field.DataSet.Post;
                            end;
                        end;
                    end;
                end;
            end;
        end
      else
        begin
          raise Exception.Create('Server not in correct record');
        end;
    end;
end;

procedure TIWBSJQGrid.DoOnRowSel(aParams: TStringList);
var
  Value:variant;
begin
  if FKeyField = '' then
    raise Exception.Create('No KeyField set. This must be a primarykey or unique field.');
  Value:= aParams.Values['key'];
  if Value = '' then Exit;

  case DataSource.DataSet.FieldByName(FKeyField).DataType of
    ftSmallint, ftInteger, ftWord,ftLongWord,
    ftShortint, ftByte: VarCast(Value, Value, varInteger);
    ftLargeint, ftAutoInc: VarCast(Value, Value, varInt64);
    ftBoolean:  VarCast(Value, Value, varBoolean);
    ftFloat:  VarCast(Value, Value, varDouble);
    ftCurrency, ftBCD, ftOraInterval, ftExtended: VarCast(Value, Value, varCurrency);
    ftDate:  VarCast(Value, Value, varDate);
    ftTime:   VarCast(Value, Value, varDate);
    ftDateTime, ftOraTimeStamp, ftTimeStampOffset: VarCast(Value, Value, varDate);
    ftBytes, ftVarBytes, ftBlob, ftGraphic, ftParadoxOle, ftDBaseOle,
    ftTypedBinary, ftCursor, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob,
    ftOraClob, ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
    ftConnection, ftParams, ftStream,ftObject, ftSingle: raise Exception.Create('Invalid Key Column for Grid ' + HTMLName);
  end;

  if Not DataSource.DataSet.Locate(FKeyField, Value, [loCaseInsensitive]) then
    begin
      gGetWebApplicationThreadVar.ShowMessage('Record not found on the server');
      gGetWebApplicationThreadVar.CallBackResponse.AddJavaScriptToExecute(
           '$("#' + HTMLName + '").trigger("reloadGrid")'
      );
    end;
end;

function TIWBSJQGrid.GetColumns: TOwnedCollection;
begin
  { if not Assigned(FColumns) then
    FColumns:= TOwnedCollection.Create(Self, TIwSrpJQGridColum); }
  Result := FColumns;
end;

function TIWBSJQGrid.GetTagType: string;
begin
  Result := inherited TagType;
end;

procedure TIWBSJQGrid.InternalRenderScript(AContext: TIWCompContext;
  const AHTMLName: string; AScript: TStringList);
begin
  inherited;
  UpdateOptions;

  AScript.Add('var w = $("#{%htmlname%}").parent().width();' +
              'w = w - 20;' +
              '$("#{%htmlname%}").jqGrid({%options%});' +

              'var lastRowSel;' +
              'function internalSelectRow(id) {' +
                  'if (id && id !== lastRowSel) {' +
                      '$("#{%htmlname%}").jqGrid(''saveRow'',lastRowSel);' +
		                  '$("#{%htmlname%}").jqGrid(''editRow'',id, {keys:true, focusField: 4});' +
		                  'lastRowSel = id;' +
                      'executeAjaxEvent("&key="+id, null, "{%htmlname%}.DoOnRowSel", true, null, true);' +
	                '}' +
              '};'
  );
  AContext.WebApplication.RegisterCallBack(AHTMLName+'.DoOnRowSel', DoOnRowSel);
end;



function TIWBSJQGrid.IsSortFieldNameStored: Boolean;
begin
  Result := FSortColumn <> '';
end;

function TIWBSJQGrid.IsTagTypeStored: Boolean;
begin
  Result := TagType <> 'table';
end;

function TIWBSJQGrid.RenderAsync(AContext: TIWCompContext): TIWXMLTag;
begin
  if FColumns.Count = 0 then
    UpdateOptions;
  Result := inherited;
end;

function TIWBSJQGrid.RenderHTML(AContext: TIWCompContext): TIWHTMLTag;
begin
  if FColumns.Count = 0 then
    UpdateOptions;
  Result := inherited;
end;


procedure TIWBSJQGrid.VerifyColumns;
var
  J: integer;
begin
  if (FColumns.Count = 0) and (Assigned(DataSource)) then
    begin
      for J := 0 to DataSource.DataSet.FieldCount - 1 do
        begin
          with TIWBSJQGridColumn(FColumns.Add) do
            begin
              FFieldName := DataSource.DataSet.Fields[J].FieldName;
              Title      := DataSource.DataSet.Fields[J].DisplayName;
            end;
        end;
    end;
end;

procedure TIWBSJQGrid.SetclickToSelect(const Value: Boolean);
begin
  if FclickToSelect <> Value then
    begin
      FclickToSelect := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetColumns(const Value: TOwnedCollection);
begin
  { if not Assigned(FColumns) then
    FColumns:= TOwnedCollection.Create(Self, TIwSrpJQGridColum); }
  if Value <> FColumns then
    begin
      FColumns.Assign(Value);
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetGroupCollapse(const Value: Boolean);
begin
  FGroupCollapse := Value;
end;

procedure TIWBSJQGrid.SetGroupingField(const Value: string);
begin
  FGroupingField := Value;
end;

procedure TIWBSJQGrid.SetGroupLabel(const Value: string);
begin
  FGroupLabel := Value;
end;

procedure TIWBSJQGrid.SetGroupSummary(const Value: Boolean);
begin
  FGroupSummary := Value;
end;

procedure TIWBSJQGrid.SetKeyField(const Value: string);
begin
  FKeyField := Value;
end;

procedure TIWBSJQGrid.SetMobileResponsive(const Value: Boolean);
begin
  if FMobileResponsive <> Value then
    begin
      FMobileResponsive := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetPagination(const Value: Boolean);
begin
  FPagination := Value;
  UpdateOptions;
end;

procedure TIWBSJQGrid.SetPaginationPosition(const Value: TBsJQGPagPosition);
begin
  if FPaginationPosition <> Value then
    begin
      FPaginationPosition := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetShowFooter(const Value: Boolean);
begin
  if FShowFooter <> Value then
    begin
      FShowFooter := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetShowHeader(const Value: Boolean);
begin
  if FShowHeader <> Value then
    begin
      FShowHeader := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetShowRefresh(const Value: Boolean);
begin
  if FShowRefresh <> Value then
    begin
      FShowRefresh := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetSingleSelect(const Value: Boolean);
begin
  if FSingleSelect <> Value then
    begin
      FSingleSelect := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetSortColumn(const Value: string);
begin
  if FSortColumn <> Value then
    begin
      FSortColumn := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetSortOrder(const Value: TBsJQGSortOrder);
begin
  if FSortOrder <> Value then
    begin
      FSortOrder := Value;
      UpdateOptions;
    end;
end;

procedure TIWBSJQGrid.SetTagType(const Value: string);
begin
  inherited TagType := Value;
  UpdateOptions;
end;

procedure TIWBSJQGrid.UpdateOptions;
var
  OptColumns:string;
  OptColumn, OptTxt: TStrings;
  J: integer;
  Field:TField;
  LGridEditable:Boolean;
begin
  VerifyColumns;
  LGridEditable:=False;
  OptColumns:='';

  OptColumn:= TStringList.Create;
  try
    OptColumn.NameValueSeparator := ':';
    OptColumn.Delimiter          := ',';
    OptColumn.QuoteChar          := ' ';
    OptColumn.StrictDelimiter    := True;

  for J      := 0 to FColumns.Count - 1 do
    begin
      OptColumn.Clear;
      with TIWBSJQGridColumn(FColumns.Items[J]) do
        begin
          OptColumn.Values['label']:= '''' +  FTitle + '''';
          OptColumn.Values['name']:= '''' + FFieldName + '''';
          if AnsiUpperCase(FFieldName) = AnsiUpperCase(FKeyField) then
            OptColumn.Values['key']:= 'true';
          if Not FVisible then
            OptColumn.Values['hidden']:='true';
          if not FSortable then
            OptColumn.Values['sortable']:='false';
          if not FResizable then
            OptColumn.Values['resizable']:='false';

          case FHeaderAlign of
            bscAlLeft:
              OptColumn.Values['labelAlign']:='"left"';
            bscAlRight:
              OptColumn.Values['labelAlign']:='"right"';
            bscAlDefault:
              OptColumn.Values['labelAlign']:='"likeData"';
           // bscAlCenter is default
          end;

          case FDataAlign of
            bscAlLeft:
              OptColumn.Values['align']:='"left"';
            bscAlRight:
              OptColumn.Values['align']:='"right"';
            bscAlCenter:
              OptColumn.Values['align']:='"center"';
          end;

          if FCSSclass <> '' then
            OptColumn.Values['classes']:= '''' + FCSSclass + '''';

          if Editable then
            begin
              LGridEditable:=True;
              if AnsiUpperCase(FKeyField) <> AnsiUpperCase(FieldName) then
                OptColumn.Values['editable']:='true'
              else
                OptColumn.Values['editable']:='false'; //KeyField is not editable

              if Assigned(DataSource.DataSet) then
                begin
                  Field:= DataSource.DataSet.FieldByName(FieldName);

                  case Field.DataType of
                    ftUnknown: ;
                    ftString: begin
                                OptColumn.Values['edittype']:='"text"';
                              end;
                    ftSmallint: begin
                                  OptColumn.Values['edittype']:='"text"';
                                end;
                    ftInteger: begin
                                  OptColumn.Values['edittype']:='"text"';
                                end;
                    ftWord: begin

                                end;
                    ftBoolean: begin
                                  OptColumn.Values['edittype']:='"checkbox"';
                                  OptColumn.Values['editoptions']:='{value:"True:False"}';
                                  OptColumn.Values['formatter']:='"checkbox"';
                                  OptColumn.Values['align']:='"center"';
                                end;
                    ftFloat: begin
                                 OptColumn.Values['edittype']:='"text"';
                                end;
                    ftCurrency: begin
                                  OptColumn.Values['edittype']:='"text"';
                                end;
                    ftBCD: begin
                               OptColumn.Values['edittype']:='"text"';
                                end;
                    ftDate: begin

                                end;
                    ftTime: begin

                                end;
                    ftDateTime: begin

                                end;
                    ftBytes: begin

                                end;
                    ftVarBytes: begin

                                end;
                    ftAutoInc: begin

                                end;
                    ftBlob: begin

                                end;
                    ftMemo: begin
                                 OptColumn.Values['edittype']:='"textarea"';
                                end;
                    ftGraphic: begin
                                  OptColumn.Values['edittype']:='"textarea"';
                                end;
                    ftFmtMemo: begin
                                 OptColumn.Values['edittype']:='"textarea"';
                                end;
                    ftParadoxOle: begin

                                end;
                    ftDBaseOle: begin

                                end;
                    ftTypedBinary: begin

                                end;
                    ftCursor: begin

                                end;
                    ftFixedChar: begin
                                   OptColumn.Values['edittype']:='"text"';
                                end;
                    ftWideString: begin
                                   OptColumn.Values['edittype']:='"text"';
                                end;
                    ftLargeint: begin
                                  OptColumn.Values['edittype']:='"text"';
                                end;
                    ftADT: begin

                                end;
                    ftArray: begin

                                end;
                    ftReference: begin

                                end;
                    ftDataSet: begin

                                end;
                    ftOraBlob: begin

                                end;
                    ftOraClob: begin

                                end;
                    ftVariant: begin
                                 OptColumn.Values['edittype']:='"textarea"';
                                end;
                    ftInterface: begin

                                end;
                    ftIDispatch: begin

                                end;
                    ftGuid: begin

                                end;
                    ftTimeStamp: begin

                                end;
                    ftFMTBcd: begin

                                end;
                    ftFixedWideChar: begin
                                  OptColumn.Values['edittype']:='"text"';
                                end;
                    ftWideMemo: begin
                                  OptColumn.Values['edittype']:='"textarea"';
                                end;
                    ftOraTimeStamp: begin

                                end;
                    ftOraInterval: begin

                                end;
                    ftLongWord: begin

                                end;
                    ftShortint: begin
                                   OptColumn.Values['edittype']:='"text"';
                                end;
                    ftByte: begin
                                OptColumn.Values['edittype']:='"text"';
                                end;
                    ftExtended: begin
                                  OptColumn.Values['edittype']:='"text"';
                                end;
                    ftConnection: begin

                                end;
                    ftParams: begin
                                 OptColumn.Values['edittype']:='"textarea"';
                                end;
                    ftStream: begin
                                 OptColumn.Values['edittype']:='"text"';
                                end;
                    ftTimeStampOffset: begin

                                end;
                    ftObject: begin

                                end;
                    ftSingle: begin

                                end;
                  end;

                end;

            end;


         (*
          case FDataVertAlign of
            bscAlVerTop:
              OptColumns := OptColumns + '","valign":"top';
            bscAlVerBottom:
              OptColumns := OptColumns + '","valign":"bottom';
            bscAlVerMiddle:
              OptColumns := OptColumns + '","valign":"middle';
          end;
          case FFooterAlign of
            bscAlLeft:
              OptColumns := OptColumns + '","falign":"left';
            bscAlRight:
              OptColumns := OptColumns + '","falign":"right';
            bscAlCenter:
              OptColumns := OptColumns + '","falign":"center';
          end;  *)
          if (FWidth <> '') then
            OptColumn.Values['width']:= FWidth;

          if OptColumns = '' then
            OptColumns := '{' + OptColumn.DelimitedText + '}'
          else
            OptColumns := OptColumns + ',{' + OptColumn.DelimitedText + '}';
        end;

    end;


  OptTxt := TStringList.Create;
  try
    OptTxt.NameValueSeparator := ':';
    OptTxt.Delimiter          := ',';
    OptTxt.QuoteChar          := ' ';
    OptTxt.StrictDelimiter    := True;
    try

      OptTxt.Values['url']              := '"'+ IWBSRegisterRestCallBack(gGetWebApplicationThreadVar, HTMLName +'.dataurl', DbJQGridCustomRestEvents0RestEvent) + '"';
    except

    end;
    OptTxt.Values['mtype'] := '"GET"';


    OptTxt.Values['datatype']  := '"jsonp"';
    //OptTxt.Values['data']       := 'mydata';
    OptTxt.Values['height']   := '200';
    OptTxt.Values['colModel']          := '[' + OptColumns + ']';
    OptTxt.Values['page']          := '1';

    OptTxt.Values['rowNum']          := '20';
    OptTxt.Values['scrollPopUp']          := 'true';
    OptTxt.Values['scrollLeftOffset']          := '"83%"';
    OptTxt.Values['viewrecords']          := 'true';
    OptTxt.Values['scroll']          := '1';
    OptTxt.Values['emptyrecords']          := '''Scroll to bottom to retrieve new page''';
    OptTxt.Values['pager']          := '"#jqGridPager"';
    OptTxt.Values['guiStyle']          := '"bootstrap"';
    OptTxt.Values['responsive']          := 'true';
    OptTxt.Values['width']          := '"auto"';
    OptTxt.Values['regional']          := '"pt-br"';
    if FGroupingField <> '' then
      begin
        OptTxt.Values['grouping']:= 'true';
        OptTxt.Values['groupingView']:= '{groupField: [''' + FGroupingField + '''],'+
                    'groupColumnShow: [true],'+
                    'groupText: ["' + FGroupLabel + '<b>{0}</b>"],'+
                    'groupOrder: ["asc"],'+
                    'groupSummary: [' + IfThen(FGroupSummary,'true','false') + '],'+
                    'groupCollapse: ' + IfThen(FGroupCollapse, 'true','false') + ',' +
                    'groupDataSorted : true }';

      end;

    if LGridEditable then
      begin
        OptTxt.Values['onSelectRow'] :=  'internalSelectRow';
        OptTxt.Values['editurl']:= '"'+ IWBSRegisterRestCallBack(gGetWebApplicationThreadVar, HTMLName +'.editurl', DoEditRow) + '"';
      end;

   (* OptTxt.Values['viewrecords']          := 'true';
    OptTxt.Values['width']          := 'w';
    OptTxt.Values['responsive']          := 'true';
    OptTxt.Values['styleUI']          := '"Bootstrap"';
    OptTxt.Values['caption']          := '"Load jqGrid through Javascript Array"';
    *)
    (*
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
    OptTxt.Values['singleSelect']  := IfThen(FSingleSelect, 'true', 'false');  *)

    ScriptParams.Values['options'] := '{' + OptTxt.DelimitedText + '}';

  finally
    OptTxt.Free;
  end;
  finally
    OptColumn.Free;
  end;
  if LGridEditable then
    begin
      TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/grid.common.js');
      TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/grid.inlinedit.js');
      TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/jquery.fmatter.js')
    end;
end;

{ TIWBSJQGridColumn }

constructor TIWBSJQGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCSSclass      := '';
  FDataAlign     := bscAlDefault;
  //FDataVertAlign := bscAlVerMiddle;
  FHeaderAlign   := bscAlCenter;
  //FFooterAlign   := bscAlLeft;
  FWidth         := '';
  FSortable      := True;
  FResizable:= True;
  FVisible:=True;
  FEditable:=True;
end;

destructor TIWBSJQGridColumn.Destroy;
begin

  inherited;
end;

function TIWBSJQGridColumn.IsCssClassStored: Boolean;
begin
  Result := FCSSclass <> '';
end;

function TIWBSJQGridColumn.IsWidthStored: Boolean;
begin
  Result := FWidth <> '';
end;

procedure TIWBSJQGridColumn.SetCSSclass(const Value: string);
begin
  if FCSSclass <> Value then
    begin
      FCSSclass := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

procedure TIWBSJQGridColumn.SetDataAlign(const Value: TBsColDataAlign);
begin
  if FDataAlign <> Value then
    begin
      FDataAlign := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

(*procedure TIWBSJQGridColumn.SetDataVertAlign(const Value: TBsColDataVertAlign);
begin
  if FDataVertAlign <> Value then
    begin
      FDataVertAlign := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end; *)

procedure TIWBSJQGridColumn.SetEditable(const Value: Boolean);
begin
  FEditable := Value;
end;

procedure TIWBSJQGridColumn.SetFieldName(const Value: string);
begin
  FFieldName := Value;
end;

(*procedure TIWBSJQGridColumn.SetFooterAlign(const Value: TBsColDataAlign);
begin
  if FFooterAlign <> Value then
    begin
      FFooterAlign := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end; *)

procedure TIWBSJQGridColumn.SetHeaderAlign(const Value: TBsColDataAlign);
begin
  if FHeaderAlign <> Value then
    begin
      FHeaderAlign := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

procedure TIWBSJQGridColumn.SetInputType(const Value: TbsJQGColInputType);
begin
  FInputType := Value;
end;


procedure TIWBSJQGridColumn.SetResizable(const Value: Boolean);
begin
  FResizable := Value;
end;

procedure TIWBSJQGridColumn.SetSortable(const Value: Boolean);
begin
  if FSortable <> Value then
    begin
      FSortable := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

procedure TIWBSJQGridColumn.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TIWBSJQGridColumn.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TIWBSJQGridColumn.SetWidth(const Value: string);
begin
  if FWidth <> Value then
    begin
      FWidth := Value;
      // (GetOwner as TIWBSJQGrid).UpdateOptions;
    end;
end;

initialization

// Enable CSS and JS for JQGrid Plugin
if DebugHook <> 0 then
  begin
    TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/ui.jqgrid.css');
    TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/jquery.jqGrid.js');
    TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/i18n/grid.locale-pt-br.js');
  end
else
  begin
    TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/ui.jqgrid.min.css');
    TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/jquery.jqGrid.min.js');
    TIWBSGlobal.IWBSAddGlobalLinkFile('/<iwbspath>/jqgrid/i18n/grid.locale-pt-br.js');
  end;

// this enable the rest event server
IWBSRegisterRestServerHandler;

end.
