unit IWBSInput;

interface

uses
  SysUtils, Classes, db, StrUtils, Controls,
  IWRenderContext, IWHTMLTag, IWXMLTag, IWBaseHTMLControl, IWBaseInterfaces, IWApplication,
  IWCompCheckBox, IWCompRadioButton, IWCompButton, IWDBStdCtrls, IWDBExtCtrls,
  IWBSRegion, IWBSCommon, IWBSCustomInput, IWBSGlobal;

type

  TIWBSInput = class(TIWBSCustomTextInput)
  private
    FMask: string;
    FAutoComplete: Boolean;
    FMaskSave: Boolean;
    procedure SetMask(const Value: string);
    procedure SetAutoComplete(const Value: Boolean);
    procedure SetMaskSave(const Value: Boolean);
  protected
    procedure InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag); override;
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property InputType default bsitText;
    property Mask:string read FMask write SetMask;
    property MaskSave:Boolean read FMaskSave write SetMaskSave default False;
    //FAutocomplete for websites it is good, but for applications it not intended
    property AutoComplete:Boolean read FAutoComplete write SetAutoComplete default False;
  end;

  TIWBSMemo = class(TIWBSCustomTextInput)
  private
    FLines: TStringList;
    FResizeDirection: TIWBSResizeDirection;
    FRows: integer;
    FVertScrollBar: boolean;
    procedure OnLinesChange(ASender : TObject);
    procedure SetLines(const AValue: TStringList);
  protected
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean); override;
    procedure InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag); override;
    procedure InternalRenderStyle(AStyle: TStringList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetText(const AValue: TCaption); override;
  published
    property Lines: TStringList read FLines write SetLines;
    property ResizeDirection: TIWBSResizeDirection read FResizeDirection write FResizeDirection default bsrdDefault;
    property Rows: integer read FRows write FRows default 5;
    property VertScrollBar: boolean read FVertScrollBar write FVertScrollBar default True;
  end;

  TIWBSCheckBox = class(TIWBSCustomInput)
  private
    FChecked: boolean;
    FValueChecked: string;
    FValueUnchecked: string;
  protected
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean); override;
    procedure InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication); override;
    procedure InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag); override;
    procedure SetChecked(AValue: boolean);
    procedure SetName(const AValue: TComponentName); override;
  public
    procedure SetText(const AValue: TCaption); override;
  published
    constructor Create(AOwner: TComponent); override;
    property Checked: boolean read FChecked write SetChecked default False;
    property ValueChecked: string read FValueChecked write FValueChecked;
    property ValueUnchecked: string read FValueUnchecked write FValueUnchecked;
  end;

  TIWBSRadioButton = class(TIWBSCustomInput)
  private
    FChecked: boolean;
    FGroup: string;
    FSaveUnchecked: boolean;
    FValueChecked: string;
    FValueUnchecked: string;
  protected
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean); override;
    procedure InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication); override;
    procedure InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag); override;
    procedure SetChecked(AValue: boolean);
    procedure SetName(const AValue: TComponentName); override;
    function InputSuffix: string; override;
  public
    procedure SetText(const AValue: TCaption); override;
  published
    constructor Create(AOwner: TComponent); override;
    property Checked: boolean read FChecked write SetChecked default False;
    property SaveUnchecked: boolean read FSaveUnchecked write FSaveUnchecked default True;
    property Group: string read FGroup write FGroup;
    property ValueChecked: string read FValueChecked write FValueChecked;
    property ValueUnchecked: string read FValueUnchecked write FValueUnchecked;
  end;

  TIWBSSelect = class(TIWBSCustomSelectInput)
  private
    FItemsSelected: array of boolean;
    FMultiSelect: boolean;
    FSize: integer;
    procedure ResetItemsSelected;
    procedure SetSize(AValue: integer);
  protected
    procedure InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean); override;
    procedure InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication); override;
    procedure InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag); override;
    procedure OnItemsChange(ASender : TObject); override;
    procedure SetItemIndex(AValue: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetText(const AValue: TCaption); override;
  published
    property MultiSelect: boolean read FMultiSelect write FMultiSelect default False;
    property Size: integer read FSize write SetSize default 1;
  end;

  TIWBSRadioGroup = class(TIWBSCustomSelectInput)
  protected
    procedure InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication); override;
    procedure InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag); override;
    function InputSelector: string; override;
    function InputSuffix: string; override;
  end;

implementation

uses IW.Common.System, IWResourceStrings, IWBSInputCommon, IWBSUtils;

{$region 'TIWBSInput'}
constructor TIWBSInput.Create(AOwner: TComponent);
begin
  inherited;
  FAutoComplete:=False;
  FMask:= '';
end;

procedure TIWBSInput.InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag);
var
  MaskTag:TIWHTMLTag;
  FakeAutocomp:TIWHTMLTag;
begin
  inherited;
  if FIsStatic then
    begin
      AHTMLTag := TIWHTMLTag.CreateTag('p');
      try
        AHTMLTag.AddClassParam(ActiveCss);
        AHTMLTag.AddStringParam('id', AHTMLName);
        AHTMLTag.AddStringParam('style', ActiveStyle);
        AHTMLTag.Contents.AddText(TextToHTML(FText));
      except
        FreeAndNil(AHTMLTag);
        raise;
      end;
    end
  else
    begin
      AHTMLTag := TIWHTMLTag.CreateTag('input');
      try
        AHTMLTag.AddClassParam(ActiveCss);
        AHTMLTag.AddStringParam('id', AHTMLName);
        AHTMLTag.AddStringParam('name', AHTMLName);
        AHTMLTag.AddStringParam('type', aIWBSInputType[InputType]);
        if ShowHint and (Hint <> '') then
          AHTMLTag.AddStringParam('title', Hint);
        if AutoFocus then
          AHTMLTag.Add('autofocus');
        if IsReadOnly then
          AHTMLTag.Add('readonly');
        if IsDisabled then
          AHTMLTag.Add('disabled');
        if MaxLength > 0 then
          AHTMLTag.AddIntegerParam('maxlength', MaxLength);
        AHTMLTag.AddStringParam('value', TextToHTML(FText));
        if Required then
          AHTMLTag.Add('required');
        if PlaceHolder <> '' then
          AHTMLTag.AddStringParam('placeholder', TextToHTML(PlaceHolder));
        if TabIndex <> 0 then
          AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
        AHTMLTag.AddStringParam('autocomplete', IfThen(FAutoComplete, 'on', 'off'));
        AHTMLTag.AddStringParam('style', ActiveStyle);
      except
        FreeAndNil(AHTMLTag);
        raise;
      end;
    end;
  if FMask <> '' then
    begin
      MaskTag:= TIWHTMLTag.CreateTag('script');
      MaskTag.Contents.AddText( '$("#' + HTMLName + '").mask("999.999.999-99",{placeholder:" "});');
      AHTMLTag.Contents.AddTagAsObject(MaskTag);
    end;

  if not (Parent is TIWBSInputGroup) and (InputType <> bsitHidden) then
    begin
      AHTMLTag := IWBSCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, AHTMLName);
      //browsers not respect autocomplete "off" in password inputs,
      //to solve this, put another input password hidden,
      //it elude browser with password change form
      if (InputType = bsitPassword) and (not FAutoComplete) then
        begin
          FakeAutocomp:= TIWHTMLTag.CreateTag('input');
          FakeAutocomp.AddStringParam('style', 'visibility: hidden;');
          FakeAutocomp.AddStringParam('type', 'password');
          if Caption <> '' then
            AHTMLTag.Contents.Insert(1, FakeAutocomp)
          else
            AHTMLTag.Contents.Insert(0, FakeAutocomp);
        end;
    end;
end;

procedure TIWBSInput.InternalSetValue(const ASubmitValue: string;
  var ATextValue: string; var ASetFieldValue: boolean);
Var
  I : Integer ;
  LenValue : Integer;
begin
  if (FMask = '') or FMaskSave then
    ATextValue := ASubmitValue
  else
    begin
      ATextValue := '' ;
      LenValue := Length( ASubmitValue ) ;
      For I := 1 to LenValue do
      begin
       if (CharIsAlphaNum(ASubmitValue[I])) or (Pos(ASubmitValue[I], FMask) = 0) then
         ATextValue := ATextValue + ASubmitValue[I];
      end;
    end;
end;

procedure TIWBSInput.SetAutoComplete(const Value: Boolean);
begin
  FAutoComplete := Value;
end;

procedure TIWBSInput.SetMask(const Value: string);
begin
  if FMask <> Value then
    begin
      FMask := Value;
      if FMask <> '' then
        TIWBSGlobal.IWBSAddGlobalLinkFile(gIWBSLibPath + '/maskedinput/jquery.maskedinput.min.js' )
      else
        TIWBSGlobal.IWBSRemoveGlobalLinkFile(gIWBSLibPath + '/maskedinput/jquery.maskedinput.min.js');
    end;
end;

procedure TIWBSInput.SetMaskSave(const Value: Boolean);
begin
  FMaskSave := Value;
end;

{$endregion}

{$region 'TIWBSMemo'}
constructor TIWBSMemo.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TStringList.Create;
  FLines.OnChange := OnLinesChange;
  FResizeDirection := bsrdDefault;
  FRows := 5;
  FVertScrollBar := True;
  Height := 101;
  Width := 121;
end;

destructor TIWBSMemo.Destroy;
begin
  FreeAndNil(FLines);
  inherited;
end;

procedure TIWBSMemo.OnLinesChange(ASender : TObject);
begin
  FText := FLines.Text;
  Invalidate;
  if Script.Count > 0 then
    AsyncRefreshControl;
end;

procedure TIWBSMemo.SetLines(const AValue: TStringList);
begin
  FLines.Assign(AValue);
end;

procedure TIWBSMemo.SetText(const AValue: TCaption);
begin
  inherited;
  FLines.Text := FText;
  FText := FLines.Text; // this autoadjust linebreaks
end;

procedure TIWBSMemo.InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean);
begin
  FLines.Text := ASubmitValue;
  ATextValue := FLines.Text;
end;

procedure TIWBSMemo.InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag);
begin
  inherited;
  AHTMLTag := TIWHTMLTag.CreateTag('textarea');
  try
    AHTMLTag.AddStringParam('id', AHTMLName);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('name', AHTMLName);
    if ShowHint and (Hint <> '') then
      AHTMLTag.AddStringParam('title', Hint);
    if AutoFocus then
      AHTMLTag.Add('autofocus');
    if IsReadOnly then
      AHTMLTag.Add('readonly');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    if MaxLength > 0 then
      AHTMLTag.AddIntegerParam('maxlength', MaxLength);
    if Required then
      AHTMLTag.Add('required');
    if PlaceHolder <> '' then
      AHTMLTag.AddStringParam('placeholder', TextToHTML(PlaceHolder));
    AHTMLTag.AddIntegerParam('rows', FRows);
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    AHTMLTag.AddStringParam('style', ActiveStyle);
    AHTMLTag.Contents.AddText(TextToHTML(FText,false,false));
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if not (Parent is TIWBSInputGroup) then
    AHTMLTag := IWBSCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, HTMLName);
end;

procedure TIWBSMemo.InternalRenderStyle(AStyle: TStringList);
begin
  if not FVertScrollBar then
    AStyle.Values['overflow'] := 'hidden';
  if FResizeDirection <> bsrdDefault then
    AStyle.Values['resize'] := aIWBSResizeDirection[FResizeDirection];
end;
{$endregion}

{$region 'TIWBSCheckBox'}
constructor TIWBSCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FChecked := False;
  FValueChecked := 'true';
  FValueUnchecked := 'false';
  FText := FValueUnchecked;
end;

procedure TIWBSCheckBox.SetName(const AValue: TComponentName);
begin
  if Caption = Name then
    Caption := AValue;
  inherited;
end;

procedure TIWBSCheckBox.SetChecked(AValue: boolean);
begin
  FChecked := AValue;
  if AValue then
    FText := FValueChecked
  else
    FText := FValueUnchecked;
  Invalidate;
end;

procedure TIWBSCheckBox.SetText(const AValue: TCaption);
begin
  inherited;
  FChecked := FText = FValueChecked;
end;

procedure TIWBSCheckBox.InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean);
begin
  FChecked := ASubmitValue = 'on';
  if FChecked then
    ATextValue := FValueChecked
  else
    ATextValue := FValueUnchecked;
end;

procedure TIWBSCheckBox.InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication);
begin
  inherited;
  if FText <> FOldText then begin
    IWBSExecuteAsyncJScript(AApplication, '$("#'+HTMLName+'").prop("checked", '+iif(Checked,'true','false')+');', False, True);
    FOldText := FText;
  end;
end;

procedure TIWBSCheckBox.InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag);
begin
  inherited;
  AHTMLTag := TIWHTMLTag.CreateTag('input');
  try
    AHTMLTag.AddStringParam('id', AHTMLName);
    AHTMLTag.AddStringParam('name', AHTMLName);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('type', 'checkbox');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    if Checked then
      AHTMLTag.Add('checked');
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    AHTMLTag.AddStringParam('style', ActiveStyle);
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if Parent is TIWBSInputGroup then
    AHTMLTag := IWBSCreateInputGroupAddOn(AHTMLTag, AHTMLName, 'addon')
  else
    AHTMLTag := IWBSCreateCheckBoxFormGroup(Parent, AHTMLTag, 'checkbox', Caption, Hint, AHTMLName, ShowHint);
end;
{$endregion}

{$region 'TIWBSRadioButton'}
constructor TIWBSRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FChecked := False;
  FGroup := 'group';
  FSaveUnchecked := True;
  FValueChecked := 'true';
  FValueUnchecked := 'false';
  FText := FValueUnchecked;
end;

function TIWBSRadioButton.InputSuffix: string;
begin
  Result := '_INPUT';
end;

procedure TIWBSRadioButton.SetName(const AValue: TComponentName);
begin
  if Caption = Name then
    Caption := AValue;
  inherited;
end;

procedure TIWBSRadioButton.SetChecked(AValue: boolean);
begin
  FChecked := AValue;
  if AValue then
    FText := FValueChecked
  else
    FText := FValueUnchecked;
  Invalidate;
end;

procedure TIWBSRadioButton.SetText(const AValue: TCaption);
begin
  inherited;
  FChecked := FText = FValueChecked;
end;

procedure TIWBSRadioButton.InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean);
begin
  if ASubmitValue = 'on' then
    ATextValue := FValueChecked
  else
    begin
      ATextValue := FValueUnchecked;
      ASetFieldValue := FSaveUnchecked;
    end;
end;

procedure TIWBSRadioButton.InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication);
begin
  inherited;
  if FText <> FOldText then begin
    IWBSExecuteAsyncJScript(AApplication, '$("#'+HTMLName+InputSuffix+'").prop("checked", '+iif(Checked,'true','false')+');', False, True);
    FOldText := FText;
  end;
end;

procedure TIWBSRadioButton.InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag);
begin
  inherited;
  AHTMLTag := TIWHTMLTag.CreateTag('input');
  try
    AHTMLTag.AddStringParam('id', AHTMLName+InputSuffix);
    AHTMLTag.AddStringParam('name', FGroup);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('type', 'radio');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    if FChecked then
      AHTMLTag.Add('checked');
    AHTMLTag.AddStringParam('onclick', 'radioButtonClick(event, '''+FGroup+''','''+AHTMLName+InputSuffix+''');');
    AHTMLTag.AddStringParam('value', 'on');
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    AHTMLTag.AddStringParam('style', ActiveStyle);
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if Parent is TIWBSInputGroup then
    AHTMLTag := IWBSCreateInputGroupAddOn(AHTMLTag, AHTMLName, 'addon')
  else
    AHTMLTag := IWBSCreateCheckBoxFormGroup(Parent, AHTMLTag, 'radio', Caption, Hint, AHTMLName, ShowHint);
end;
{$endregion}

{$region 'TIWBSSelect'}
constructor TIWBSSelect.Create(AOwner: TComponent);
begin
  inherited;
  FMultiSelect := False;
  FSize := 1;
end;

procedure TIWBSSelect.OnItemsChange(ASender : TObject);
begin
  inherited;
  SetLength(FItemsSelected, Items.Count);
  ResetItemsSelected;
end;

procedure TIWBSSelect.SetItemIndex(AValue: integer);
begin
  if not FMultiSelect and (AValue < 0) and (Items.Count > 0) then
    AValue := 0;
  inherited;
  ResetItemsSelected;
end;

procedure TIWBSSelect.ResetItemsSelected;
var
  i: integer;
begin
  for i := 0 to Length(FItemsSelected)-1 do
    FItemsSelected[i] := false;
  if (FItemIndex >= 0) and (FItemIndex < Length(FItemsSelected)) then
    FItemsSelected[FItemIndex] := True;
end;

procedure TIWBSSelect.SetSize(AValue: integer);
begin
  FSize := AValue;
  Invalidate;
end;

procedure TIWBSSelect.SetText(const AValue: TCaption);
var
  LSelectedVal: TStringList;
  i, j: integer;
begin
  FText := AValue;
  if FMultiSelect and AnsiContainsStr(FText,',') then
    begin
      ResetItemsSelected;
      LSelectedVal := TStringList.Create;
      try
        LSelectedVal.StrictDelimiter := True;
        LSelectedVal.CommaText := FText;
        for i := 0 to LSelectedVal.Count-1 do
          for j := 0 to Items.Count-1 do
            if AnsiSameStr(IfThen(ItemsHaveValues, Items.ValueFromIndex[j], Items[j]), LSelectedVal[i]) then
              FItemsSelected[j] := True;
      finally
        LSelectedVal.Free;
      end;
    end
  else
    begin
      FItemIndex := FindValue(FText);
      if not FMultiSelect and (FItemIndex < 0) and (Items.Count > 0) then begin
        FItemIndex := 0;
        FText := IfThen(ItemsHaveValues, Items.ValueFromIndex[FItemIndex], Items[FItemIndex]);
      end;
      ResetItemsSelected;
    end;
  Invalidate;
end;

procedure TIWBSSelect.InternalSetValue(const ASubmitValue: string; var ATextValue: string; var ASetFieldValue: boolean);
var
  LSelectedIdx, LSelectedVal: TStringList;
  i, v: integer;
begin
  if FMultiSelect and AnsiContainsStr(ASubmitValue,',') then
    begin
      FItemIndex := -1;
      ResetItemsSelected;
      LSelectedIdx := TStringList.Create;
      LSelectedVal := TStringList.Create;
      try
        LSelectedIdx.CommaText := ASubmitValue;
        for i := 0 to LSelectedIdx.Count-1 do
          if TryStrToInt(LSelectedIdx[i], v) and (v >= 0) and (v < Items.Count) then begin
            if i = 0 then
              FItemIndex := v
            else if ItemsHaveValues then
              LSelectedVal.Add(Items.ValueFromIndex[v])
            else
              LSelectedVal.Add(Items[v]);
            FItemsSelected[v] := True;
          end;
        LSelectedVal.StrictDelimiter := True;
        ATextValue := LSelectedVal.CommaText;
      finally
        LSelectedIdx.Free;
        LSelectedVal.Free;
      end;
    end
  else
    begin
      inherited InternalSetValue(ASubmitValue, ATextValue, ASetFieldValue);
      ResetItemsSelected;
    end;
end;

procedure TIWBSSelect.InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication);
var
  LSelectedIdx: string;
  i: integer;
begin
  inherited;
  if (FText <> FOldText) then begin
    LSelectedIdx := '';
    if FMultiSelect then
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
  end;
end;

procedure TIWBSSelect.InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag);
var
  i: Integer;
begin
  inherited;
  AHTMLTag := TIWHTMLTag.CreateTag('select');
  try
    AHTMLTag.AddStringParam('id', AHTMLName);
    AHTMLTag.AddClassParam(ActiveCss);
    AHTMLTag.AddStringParam('name', AHTMLName);
    if FSize > 0 then
      AHTMLTag.AddIntegerParam('size', FSize)
    else
      AHTMLTag.AddIntegerParam('size', Items.Count);
    if FMultiSelect then
      AHTMLTag.Add('multiple');
    if IsDisabled then
      AHTMLTag.Add('disabled');
    if AutoFocus then
      AHTMLTag.Add('autofocus');
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    for i := 0 to Items.Count - 1 do begin
      with AHTMLTag.Contents.AddTag('option') do begin
        AddStringParam('value', IntToStr(i));
        if FItemsSelected[i] then
          Add('selected');
        Contents.AddText(TextToHTML(iif(ItemsHaveValues, Items.Names[i], Items[i])));
      end;
    end;
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if not (Parent is TIWBSInputGroup) then
    AHTMLTag := IWBSCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, AHTMLName);
end;
{$endregion}

{$region 'TIWBSRadioGroup'}
function TIWBSRadioGroup.InputSelector: string;
begin
  Result := ' input';
end;

function TIWBSRadioGroup.InputSuffix: string;
begin
  Result := '_INPUT';
end;

procedure TIWBSRadioGroup.InternalRenderAsync(const AHTMLName: string; AApplication: TIWApplication);
begin
  inherited;
  if (FText <> FOldText) then begin
    if FItemIndex >= 0 then
      IWBSExecuteAsyncJScript(AApplication, '$("#'+AHTMLName+'_INPUT_'+IntToStr(FItemIndex)+'").prop("checked", true);', False, True)
    else
      IWBSExecuteAsyncJScript(AApplication, '$("#'+AHTMLName+' input").prop("checked", false);', False, True);
    FOldText := FText;
  end;
end;

procedure TIWBSRadioGroup.InternalRenderHTML(const AHTMLName: string; AContext: TIWCompContext; var AHTMLTag: TIWHTMLTag);
var
  i: Integer;
begin
  inherited;
  AHTMLTag := TIWHTMLTag.CreateTag('div');
  try
    AHTMLTag.AddStringParam('id', AHTMLName);
    AHTMLTag.AddClassParam('radio');
    if TabIndex <> 0 then
      AHTMLTag.AddStringParam('tabindex', IntToStr(TabIndex));
    AHTMLTag.AddStringParam('style', ActiveStyle);
    for i := 0 to Items.Count - 1 do begin
      with AHTMLTag.Contents.AddTag('label') do begin
        with Contents.AddTag('input') do begin
          AddStringParam('type', 'radio');
          Add(iif(FItemIndex = i, 'checked'));
          AddStringParam('name', AHTMLName + InputSuffix);
          AddStringParam('id', AHTMLName + InputSuffix+'_'+IntToStr(i));
          AddStringParam('value', IntToStr(i));
          if IsDisabled then
            Add('disabled');
        end;
        Contents.AddText(TextToHTML(iif(ItemsHaveValues, Items.Names[i], Items[i])));
      end;
      AHTMLTag.Contents.AddText('<br>');
    end;
  except
    FreeAndNil(AHTMLTag);
    raise;
  end;

  if Parent is TIWBSInputGroup then
    AHTMLTag := IWBSCreateInputGroupAddOn(AHTMLTag, AHTMLName, 'addon')
  else
    AHTMLTag := IWBSCreateInputFormGroup(Self, Parent, AHTMLTag, Caption, AHTMLName);
end;
{$endregion}

end.
