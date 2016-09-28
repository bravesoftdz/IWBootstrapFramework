unit IWBSValidator;

interface

uses Classes, IWControl, IWHTMLTag, IWBSUtils, IWForm, Forms, IWBSCommon, IWBSRegion;

type

  TIWBSValRuleType = (tbsvalRequired, tbsvalEmail, tbsvalUrl, tbsvalNumber, tbsvalTel, tbsvalDate);
  TIWBSValidation = class;

  TIWBSValidationRuleClass = class of TIWBSValidationRule;
  TIWBSValidationRule = class(TPersistent)
    private
      FValidation:TIWBSValidation;
    protected
    procedure RenderRuleTag(aHTMLTag:TIWHTMLTag); virtual;
    public
     property Validation: TIWBSValidation read FValidation;
  end;

  TIWBSValidationHack = class(TIWBSValidationRule)
    // This Hack are necessary to initialize TIWBSValidation Class
    // is necessary decarate all property off all TIWBSValidationRule descendants
    // on this class.
    { TODO 1 : View how to solve Access Violation on create TIWBSValidation in DesignMode and remove this Hack }
  private
    FMax: Integer;
    FMin: Integer;
    FValidateRange: Boolean;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetValidateRange(const Value: Boolean);
  published
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
     property ValidateRange:Boolean read FValidateRange write SetValidateRange default False;
  end;

  TIWBSValidationRuleNumber = class(TIWBSValidationRule)
  private
    FMax: Integer;
    FMin: Integer;
    FValidateRange: Boolean;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetValidateRange(const Value: Boolean);
  protected
    procedure RenderRuleTag(aHTMLTag:TIWHTMLTag); override;
    public
      constructor Create;
  published
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property ValidateRange:Boolean read FValidateRange write SetValidateRange default False;
  end;

  TIWBSValidation = class(TCollectionItem)
  private
    FRule: TIWBSValidationRule;
    FRuleType: TIWBSValRuleType;
    FRuleClass: TIWBSValidationRuleClass;
    FErrorMsg: string;
    FShowErrorMsg: Boolean;
    procedure SetRule(const Value: TIWBSValidationRule);
    procedure SetRuleType(const Value: TIWBSValRuleType);
    function GetRule: TIWBSValidationRule;
    function GetRuleClass: TIWBSValidationRuleClass;
    procedure SetErrorMsg(const Value: string);
    procedure SetShowErrorMsg(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property RuleType: TIWBSValRuleType read FRuleType write SetRuleType;
    property Rule: TIWBSValidationRule read GetRule write SetRule;
    property ErrorMsg:string read FErrorMsg write SetErrorMsg;
    property ShowErrorMsg:Boolean read FShowErrorMsg write SetShowErrorMsg default False;
  end;

  TIWBSValidator   = class;
  TIWBSValidationClass = class of TIWBSValidation;

  TIWBSValidations = class(TCollection)
  private
    FComp: TIWBSValidator;
    function GetValidation(Index: Integer): TIWBSValidation;
    procedure SetValidation(Index: Integer; const Value: TIWBSValidation);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Comp: TIWBSValidator; ItemClass: TIWBSValidationClass);
    function Add: TIWBSValidation;
    property Comp: TIWBSValidator read FComp;
    property Items[Index: Integer]: TIWBSValidation read GetValidation write SetValidation; default;

  end;

  TIWBSValidator = class(TComponent)
  private
   // FControl: TIWBSCustomInput;
    FValidations: TIWBSValidations;
    procedure SetValidations(const Value: TIWBSValidations);

   // procedure SetControl(const Value: TIWBSCustomInput);
  protected
    //function GetDisplayName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RenderValidation(aHTMLTag:TIWHTMLTag);
     procedure Validate(aForm: TIWBSCustomInputForm = nil);
     //procedure Update(aForm: TIWBSCustomInputForm = nil);
  published
    //property Control: TIWBSCustomInput read FControl write SetControl;
    property Validations: TIWBSValidations read FValidations write SetValidations;
  end;

 (* TIWBSValidator          = class;
  TIWBSValidadorCompClass = class of TIWBSValidadorComp;

  TIWBSValidadorComps = class(TCollection)
  private
    FValidator: TIWBSValidator;
    function GetValComp(Index: Integer): TIWBSValidadorComp;
    procedure SetValComp(Index: Integer; const Value: TIWBSValidadorComp);
  protected
    function GetOwner: TPersistent; override;

  public
    constructor Create(Validator: TIWBSValidator; ItemClass: TIWBSValidadorCompClass);
    function Add: TIWBSValidadorComp;
    property Validator: TIWBSValidator read FValidator;
    property Items[Index: Integer]: TIWBSValidadorComp read GetValComp write SetValComp; default;
  end;

  TIWBSValidator = class(TComponent)
  private
    FFocusOnError: Boolean;
    FCustomValidations: string;
    FFeedbackErrorIcon: string;
    FFeedbackSucessIcon: string;
    FPaternErrorMsg: string;
    FRequiredErrorMsg: string;
    FAllErrorMsg: string;
    FMatchErrorMsg: string;
    FComps: TIWBSValidadorComps;
    procedure SetFocusOnError(const Value: Boolean);
    procedure SetCustomValidations(const Value: string);
    procedure SetFeedbackErrorIcon(const Value: string);
    procedure SetFeedbackSucessIcon(const Value: string);
    procedure SetAllErrorMsg(const Value: string);
    procedure SetMatchErrorMsg(const Value: string);
    procedure SetPaternErrorMsg(const Value: string);
    procedure SetRequiredErrorMsg(const Value: string);
    procedure SetComps(const Value: TIWBSValidadorComps);
    function CreateComps: TIWBSValidadorComps;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Validate(aForm: IIWBSContainer = nil);
    procedure RenderValidation(aInput:TIWBSCustomInput; aHTMLTag:TIWHTMLTag);
  published
    property FocusOnError: Boolean read FFocusOnError write SetFocusOnError;
    property FeedbackSucessIcon: string read FFeedbackSucessIcon write SetFeedbackSucessIcon;
    property FeedbackErrorIcon: string read FFeedbackErrorIcon write SetFeedbackErrorIcon;
    property CustomValidations: string read FCustomValidations write SetCustomValidations;
    property PaternErrorMsg: string read FPaternErrorMsg write SetPaternErrorMsg;
    property RequiredErrorMsg: string read FRequiredErrorMsg write SetRequiredErrorMsg;
    property MatchErrorMsg: string read FMatchErrorMsg write SetMatchErrorMsg;
    property AllErrorMsg: string read FAllErrorMsg write SetAllErrorMsg;
    property Comps: TIWBSValidadorComps read FComps write SetComps;
  end; *)

implementation

uses
  IWBSGlobal, System.SysUtils, System.TypInfo, Winapi.Windows;

{ TIWBSValidator }

(*constructor TIWBSValidator.Create(AOwner: TComponent);
begin
  inherited;
  FComps              := CreateComps;
  FocusOnError        := True;
  FCustomValidations  := '';
  FFeedbackErrorIcon  := 'glyphicon-remove';
  FFeedbackSucessIcon := 'glyphicon-ok';
  FPaternErrorMsg     := 'The value does not match the accepted pattern.';
  FRequiredErrorMsg   := 'Fill this field.';
  FAllErrorMsg        := 'This field is not filled correctly';
  FMatchErrorMsg      := 'Whoops, these don''t match';


end;

function TIWBSValidator.CreateComps: TIWBSValidadorComps;
begin
  Result := TIWBSValidadorComps.Create(Self, TIWBSValidadorComp);
end;

procedure TIWBSValidator.RenderValidation(aInput:TIWBSCustomInput; aHTMLTag:TIWHTMLTag);
var
  I: Integer;
begin
  for I := 0 to FComps.Count -1 do
    begin
      if FComps[I].FControl = aInput then
        begin
          FComps[I].RenderValidationsForControl(aHTMLTag);
          Break;
        end;
    end;
end;

procedure TIWBSValidator.SetAllErrorMsg(const Value: string);
begin
  FAllErrorMsg := Value;
end;

procedure TIWBSValidator.SetComps(const Value: TIWBSValidadorComps);
begin
  FComps.Assign(Value);
end;

procedure TIWBSValidator.SetCustomValidations(const Value: string);
begin
  FCustomValidations := Value;
end;

procedure TIWBSValidator.SetFeedbackErrorIcon(const Value: string);
begin
  FFeedbackErrorIcon := Value;
end;

procedure TIWBSValidator.SetFeedbackSucessIcon(const Value: string);
begin
  FFeedbackSucessIcon := Value;
end;

procedure TIWBSValidator.SetFocusOnError(const Value: Boolean);
begin
  FFocusOnError := Value;
end;

procedure TIWBSValidator.SetMatchErrorMsg(const Value: string);
begin
  FMatchErrorMsg := Value;
end;

procedure TIWBSValidator.SetPaternErrorMsg(const Value: string);
begin
  FPaternErrorMsg := Value;
end;

procedure TIWBSValidator.SetRequiredErrorMsg(const Value: string);
begin
  FRequiredErrorMsg := Value;
end; *)

procedure TIWBSValidator.Validate(aForm: TIWBSCustomInputForm = nil);
var
  i:Integer;
  LValidator:TIWBSValidator;
begin
   if (csDesigning in ComponentState) or (csLoading in ComponentState) then
     Exit;
  if Assigned(aForm) then
    begin
      IWBSExecuteAsyncJScript('$("#'+ aForm.HTMLName +'").validator(''validate'');');
    end
  else if(Owner is TIWForm)
  or ((Owner is TFrame)) then
    begin
      for i := 0 to Owner.ComponentCount -1 do
        begin
          if (Owner.Components[I] is TIWBSCustomInputForm) then
            if TIWBSCustomInputForm(Owner.Components[I]).ValidationEnabled then
               IWBSExecuteAsyncJScript('$("#'+ TIWBSCustomInputForm(Owner.Components[I]).HTMLName +'").validator(''validate'');');
        end;
    end
  else
    raise Exception.Create('IWBSValidator need to be Ownned for TIWForm or TFrame');
end;

{ TIWBSValidadorComp }

constructor TIWBSValidator.Create(AOwner: TComponent);
begin
  FValidations := TIWBSValidations.Create(Self, TIWBSValidation);
  inherited;
end;

(*function TIWBSValidadorComp.GetDisplayName: string;
begin
  if Assigned(FControl) then
    Result:= FControl.Name
  else
    inherited;
end; *)

procedure TIWBSValidator.RenderValidation(aHTMLTag:TIWHTMLTag);
var
 I:Integer;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
     Exit;
  for I := 0 to FValidations.Count -1 do
    begin
      FValidations[I].Rule.RenderRuleTag(aHTMLTag);
    end;
end;

(*procedure TIWBSValidadorComp.SetControl(const Value: TIWBSCustomInput);
begin
  FControl := Value;
end;   *)

procedure TIWBSValidator.SetValidations(const Value: TIWBSValidations);
begin
  FValidations.Assign(Value);
end;

(*procedure TIWBSValidator.Update(aForm: TIWBSCustomInputForm);
var
  i:Integer;
  LValidator:TIWBSValidator;
begin
  if Assigned(aForm) then
    begin
       IWBSExecuteAsyncJScript('$("#'+ aForm.HTMLName +'").validator(''update'');');
    end
  else if(Owner is TIWForm)
  or ((Owner is TFrame)) then
    begin
      for i := 0 to Owner.ComponentCount -1 do
        begin
          if (Owner.Components[I] is TIWBSCustomInputForm) then
            if TIWBSCustomInputForm(Owner.Components[I]).ValidationEnabled then
               IWBSExecuteAsyncJScript('$("#'+ TIWBSCustomInputForm(Owner.Components[I]).HTMLName +'").validator(''update'');');
        end;
    end
  else
    raise Exception.Create('IWBSValidator need to be Ownned for TIWForm or TFrame');

end;*)

{ TIWBSValidadorComps }

(*function TIWBSValidadorComps.Add: TIWBSValidadorComp;
begin
  Result := TIWBSValidadorComp(inherited Add);
end;

constructor TIWBSValidadorComps.Create(Validator: TIWBSValidator;
  ItemClass: TIWBSValidadorCompClass);
begin
  inherited Create(ItemClass);
  FValidator := Validator;
end;

function TIWBSValidadorComps.GetOwner: TPersistent;
begin
  Result := FValidator;
end;

function TIWBSValidadorComps.GetValComp(Index: Integer): TIWBSValidadorComp;
begin
  Result := TIWBSValidadorComp(inherited Items[Index]);
end;

procedure TIWBSValidadorComps.SetValComp(Index: Integer; const Value: TIWBSValidadorComp);
begin
  Items[Index].Assign(Value);
end;  *)

{ TIWBSValidations }

function TIWBSValidations.Add: TIWBSValidation;
begin
  Result := TIWBSValidation(inherited Add);
end;

constructor TIWBSValidations.Create(Comp: TIWBSValidator; ItemClass: TIWBSValidationClass);
begin
  inherited Create(ItemClass);
  FComp := Comp;
end;

function TIWBSValidations.GetOwner: TPersistent;
begin
  Result := FComp;
end;

function TIWBSValidations.GetValidation(Index: Integer): TIWBSValidation;
begin
  Result := TIWBSValidation(inherited Items[Index]);
end;

procedure TIWBSValidations.SetValidation(Index: Integer; const Value: TIWBSValidation);
begin
  Items[Index].Assign(Value);
end;

{ TIWBSValidation }

constructor TIWBSValidation.Create(Collection: TCollection);
begin
  inherited;
  FRuleType  := tbsvalRequired;
  FRuleClass := TIWBSValidationHack;
  FRule      := TIWBSValidationHack.Create;
  FRule.FValidation:=Self;
  FErrorMsg:= '';
  FShowErrorMsg:= False;
end;

function TIWBSValidation.GetDisplayName: string;
begin
  case FRuleType of
    tbsvalRequired: Result:= 'Required';
    tbsvalEmail:  Result:= 'Email';
    tbsvalUrl:  Result:= 'URL';
    tbsvalNumber:  Result:= 'Number';
    tbsvalTel:  Result:= 'Fone';
    tbsvalDate:  Result:= 'Date';
  else
    inherited;
  end;
end;

function TIWBSValidation.GetRule: TIWBSValidationRule;
begin
  if FRule <> nil then
    if FRule.ClassType <> FRuleClass then
      FreeAndNil(FRule);
  if FRule = nil then
    FRule := FRuleClass.Create;
  Result  := (FRule as FRuleClass);
end;

function TIWBSValidation.GetRuleClass: TIWBSValidationRuleClass;
begin
  case FRuleType of
    tbsvalNumber:
      Result := TIWBSValidationRuleNumber;
  else
    Result := TIWBSValidationRule;
  end;
end;

procedure TIWBSValidation.SetErrorMsg(const Value: string);
begin
  FErrorMsg := Value;
end;

procedure TIWBSValidation.SetRule(const Value: TIWBSValidationRule);
begin
  if Value = nil then
    FRule := nil
  else { if Value.ClassType = FRuleClass then }
    begin
      FRule.Assign(Value);
      FRuleClass := TIWBSValidationRuleClass(Value.ClassType);
      FRule.FValidation:=Self;
    end;
end;

procedure TIWBSValidation.SetRuleType(const Value: TIWBSValRuleType);
begin
  if FRuleType <> Value then
    begin
      FRuleType  := Value;
      FRuleClass := GetRuleClass;
      if FRule = nil then
        FRule := FRuleClass.Create
      else if FRule.ClassType <> FRuleClass then
        begin
          FreeAndNil(FRule);
          FRule := FRuleClass.Create;
          try
            FindRootDesigner(Self).Notification(Self, opRemove);
            FindRootDesigner(Self).Notification(Self, opInsert);
            // FindRootDesigner(Self).Modified;
          except
            // To avoid DesignTime Errors
          end;
        end;
    end;
end;

procedure TIWBSValidation.SetShowErrorMsg(const Value: Boolean);
begin
  FShowErrorMsg := Value;
end;

{ TIWBSValidationRuleNumber }

constructor TIWBSValidationRuleNumber.Create;
begin
  inherited Create;
  FValidateRange:=False;
end;

procedure TIWBSValidationRuleNumber.RenderRuleTag(aHTMLTag: TIWHTMLTag);
begin
  if aHTMLTag.Params.Values['type'] <> 'number' then
    aHTMLTag.AddStringParam('type', 'number');
  if FValidateRange then
    begin
      if aHTMLTag.Params.Values['min'] <> IntToStr(FMin) then
        aHTMLTag.AddStringParam('min', IntToStr(FMin));
      if aHTMLTag.Params.Values['max'] <> IntToStr(FMax) then
        aHTMLTag.AddStringParam('max', IntToStr(FMax));
    end;
end;

procedure TIWBSValidationRuleNumber.SetMax(const Value: Integer);
begin
  FMax := Value;
end;

procedure TIWBSValidationRuleNumber.SetMin(const Value: Integer);
begin
  FMin := Value;
end;

procedure TIWBSValidationRuleNumber.SetValidateRange(const Value: Boolean);
begin
  FValidateRange := Value;
end;

{ TIWBSValidationHack }

procedure TIWBSValidationHack.SetMax(const Value: Integer);
begin
  FMax:= Value;
end;

procedure TIWBSValidationHack.SetMin(const Value: Integer);
begin
  FMin:=Value;
end;

procedure TIWBSValidationHack.SetValidateRange(const Value: Boolean);
begin
  FValidateRange:= Value;
end;

{ TIWBSValidationRule }

procedure TIWBSValidationRule.RenderRuleTag(aHTMLTag: TIWHTMLTag);
begin
  case FValidation.RuleType of
    tbsvalRequired: begin
                      if aHTMLTag.Params.IndexOfName('required') = -1 then
                        aHTMLTag.Add('required');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-required-error', 'Please fill this field.')
                        else
                         aHTMLTag.AddStringParam('data-required-error', FValidation.ErrorMsg);
                    end;
    tbsvalEmail:    begin
                      if aHTMLTag.Params.Values['type'] <> 'email' then
                        aHTMLTag.AddStringParam('type', 'email');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-error', 'This email is not valid.')
                        else
                         aHTMLTag.AddStringParam('data-error', FValidation.ErrorMsg);
                    end;
    tbsvalUrl:      begin
                      if aHTMLTag.Params.Values['type'] <> 'url' then
                        aHTMLTag.AddStringParam('type', 'url');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-error', 'This url is not valid.')
                        else
                         aHTMLTag.AddStringParam('data-error', FValidation.ErrorMsg);
                    end;
    //tbsvalNumber: ;
    tbsvalTel:      begin
                      if aHTMLTag.Params.Values['type'] <> 'tel' then
                        aHTMLTag.AddStringParam('type', 'tel');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-error', 'This fone is not valid.')
                        else
                         aHTMLTag.AddStringParam('data-error', FValidation.ErrorMsg);
                    end;
    tbsvalDate:     begin
                      if aHTMLTag.Params.Values['type'] <> 'date' then
                        aHTMLTag.AddStringParam('type', 'date');
                      if FValidation.ShowErrorMsg then
                        if FValidation.ErrorMsg = '' then
                          aHTMLTag.AddStringParam('data-error', 'This date is not valid.')
                        else
                         aHTMLTag.AddStringParam('data-error', FValidation.ErrorMsg);
                    end;
  end;
end;

initialization
 IWBSAddGlobalLinkFile('/<iwbspath>/validator/validator.js');


end.
