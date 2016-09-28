unit IWBSInputCommon;

interface

uses Classes, SysUtils, SyncObjs, Controls,
     IWVCLBaseControl, IWBSRegion,
     IWRenderContext, IWHTMLTag;

function IWBSCreateFormGroup(AParent: TControl; AParentForm: TIWBSCustomInputForm; ATag: TIWHTMLTag; const AHTMLName: string; ASpanDiv: boolean): TIWHTMLTag;
function IWBSCreateInputFormGroup(AControl, AParent: TControl; ATag: TIWHTMLTag; const ACaption, AHTMLName: string): TIWHTMLTag;
function IWBSCreateCheckBoxFormGroup(AParent: TControl; ATag: TIWHTMLTag; const ACss, ACaption, AHint, AHTMLName: string; AShowHint: boolean): TIWHTMLTag;
function IWBSCreateInputGroupAddOn(ATag: TIWHTMLTag; const AHTMLName, css: string): TIWHTMLTag;
function CharIsNum(const C: Char): Boolean;
function CharIsAlpha(const C: Char): Boolean;
function CharIsAlphaNum(const C: Char): Boolean;


implementation

uses IWBSRegionCommon, IWBaseHTMLControl;


{$region 'String functions'}

function CharIsNum(const C: Char): Boolean;
begin
  Result := ( C in ['0'..'9'] ) ;
end ;

function CharIsAlpha(const C: Char): Boolean;
begin
  Result := ( C in ['A'..'Z','a'..'z'] ) ;
end ;

function CharIsAlphaNum(const C: Char): Boolean;
begin
  Result := ( CharIsAlpha( C ) or CharIsNum( C ) );
end ;

{$endregion}


{$region 'FormGroup functions'}
function IWBSCreateFormGroup(AParent: TControl; AParentForm: TIWBSCustomInputForm; ATag: TIWHTMLTag; const AHTMLName: string; ASpanDiv: boolean): TIWHTMLTag;
var
  LSpanDiv: TIWHTMLTag;
begin
  if (AParentForm <> nil) and not ((AParent is TIWBSRegion) and (TIWBSRegion(AParent).BSRegionType in [bsrtFormGroup,bsrtButtonGroup])) then
    begin
      Result := TIWHTMLTag.CreateTag('div');
      Result.AddClassParam('form-group');
      Result.AddStringParam('id',AHTMLName+'_FG');
      if ASpanDiv
      and (AParentForm is TIWBSInputForm)
      and (TIWBSInputForm(AParentForm).BSFormType = bsftHorizontal) then
        begin
          LSpanDiv := Result.Contents.AddTag('div');
          LSpanDiv.AddClassParam(TIWBSInputForm(AParentForm).BSFormOptions.GetOffsetClassString);
          LSpanDiv.Contents.AddTagAsObject(aTag);
        end
      else
        Result.Contents.AddTagAsObject(aTag);
    end
  else
    Result := ATag;
end;

function IWBSCreateInputFormGroup(AControl, AParent: TControl; ATag: TIWHTMLTag; const ACaption, AHTMLName: string): TIWHTMLTag;
var
  lablTag, editTag: TIWHTMLTag;
  InputForm: TIWBSCustomInputForm;
begin
  InputForm := IWBSFindParentInputForm(AParent);
  if ACaption <> '' then
    begin
      Result := TIWHTMLTag.CreateTag('div');
      try
        Result.AddClassParam('form-group');
        Result.AddStringParam('id',AHTMLName+'_FG');
        lablTag := Result.Contents.AddTag('label');
        lablTag.AddClassParam('control-label');
        lablTag.AddStringParam('for', AHTMLName);
        lablTag.Contents.AddText(TIWBaseHTMLControl.TextToHTML(ACaption));
        if (InputForm <> nil) and (InputForm is TIWBSInputForm) and (TIWBSInputForm(InputForm).BSFormType = bsftHorizontal) then
          begin
            lablTag.AddClassParam(TIWBSInputForm(InputForm).BSFormOptions.CaptionsSize.GetClassString);
            editTag := Result.Contents.AddTag('div');
            editTag.AddClassParam(TIWBSInputForm(InputForm).BSFormOptions.InputsSize.GetClassString);
            editTag.Contents.AddTagAsObject(aTag);
          end
        else
          Result.Contents.AddTagAsObject(ATag);
      except
        FreeAndNil(Result);
        FreeAndNil(ATag);
        raise;
      end;
    end
  else
    Result := IWBSCreateFormGroup(AParent, InputForm, ATag, AHTMLName, True);
end;

function IWBSCreateCheckBoxFormGroup(AParent: TControl; ATag: TIWHTMLTag; const ACss, ACaption, AHint, AHTMLName: string; AShowHint: boolean): TIWHTMLTag;
var
  lablTag: TIWHTMLTag;
  InputForm: TIWBSCustomInputForm;
begin
  InputForm := IWBSFindParentInputForm(AParent);
  Result := TIWHTMLTag.CreateTag('div');
  try
    Result.AddStringParam('id', AHTMLName+'_FG');
    if (InputForm <> nil)
    and (InputForm is TIWBSInputForm)
    and (TIWBSInputForm(InputForm).BSFormType = bsftInline) then
      Result.AddClassParam(ACss+'-inline')
    else
      Result.AddClassParam(ACss);
    if (InputForm <> nil)
    and (InputForm is TIWBSInputForm)
    and (TIWBSInputForm(InputForm).BSFormType = bsftHorizontal) then
      Result.AddClassParam(TIWBSInputForm(InputForm).BSFormOptions.GetOffsetClassString);
    lablTag := Result.Contents.AddTag('label');
    lablTag.AddStringParam('id', AHTMLName+'_CHKBCAPTION');
    if AShowHint and (AHint <> '') then
      lablTag.AddStringParam('title', AHint);
    lablTag.Contents.AddTagAsObject(ATag);
    lablTag.Contents.AddText(TIWBaseHTMLControl.TextToHTML(ACaption));

    Result := IWBSCreateFormGroup(AParent, InputForm, Result, AHTMLName, False);
  except
    FreeAndNil(Result);
    FreeAndNil(ATag);
    raise;
  end;
end;
{$endregion}

{$region 'InputGroup functions'}
function IWBSCreateInputGroupAddOn(ATag: TIWHTMLTag; const AHTMLName, css: string): TIWHTMLTag;
begin
  Result := TIWHTMLTag.CreateTag('span');
  try
    Result.AddClassParam('input-group-'+css);
    Result.Contents.AddTagAsObject(ATag);
  except
    FreeAndNil(Result);
    FreeAndNil(ATag);
    raise;
  end;
end;
{$endregion}

end.
