unit IWBSFluidForm;

interface

uses
  IWBSRegion, System.Classes, IWRenderContext, IWHTMLTag, IW.HTTP.Request,
  IWApplication, IW.HTTP.Reply, IWBSRestServer;


  type

  TIWBSFormEncType = (iwbsfeDefault, iwbsfeMultipart, iwbsfeText);

  TIWBSFluidForm = class(TIWBSCustomRegion)
  private
    FEncType: TIWBSFormEncType;
    FOnSubmit: TIWBSInputFormSubmitEvent;
    procedure DoSubmit(aApplication: TIWApplication; aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
  protected
    procedure InternalRenderCss(var ACss: string); override;
    function RenderHTML(AContext: TIWCompContext): TIWHTMLTag; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRoleString: string; override;
  published
    property EncType: TIWBSFormEncType read FEncType write FEncType default iwbsfeDefault;
    property OnSubmit: TIWBSInputFormSubmitEvent read FOnSubmit write FOnSubmit;
  end;


implementation

uses
  IWBSCommon, System.SysUtils;

{ TIWBSFluidForm }

constructor TIWBSFluidForm.Create(AOwner: TComponent);
begin
  inherited;
  FEncType := iwbsfeDefault;
  FTagType := 'form';
end;

destructor TIWBSFluidForm.Destroy;
begin

  inherited;
end;

procedure TIWBSFluidForm.DoSubmit(aApplication: TIWApplication;
  aRequest: THttpRequest; aReply: THttpReply; aParams: TStrings);
begin
  if Assigned(FOnSubmit) then
    FOnSubmit(aRequest, aParams);
  aReply.SendRedirect(aApplication.SessionInternalUrlBase);
end;

function TIWBSFluidForm.GetRoleString: string;
begin
  Result := 'form';
end;

procedure TIWBSFluidForm.InternalRenderCss(var ACss: string);
begin
  TIWBSCommon.AddCssClass(ACss, 'iwbs-form-fluid');
end;

function TIWBSFluidForm.RenderHTML(AContext: TIWCompContext): TIWHTMLTag;
var
  LParentForm: TIWBSInputForm;
begin
  LParentForm := IWBSFindParentInputForm(Parent);
  if LParentForm <> nil then
    raise Exception.Create('forms can not be nested, you try to put '+Name+' inside '+LParentForm.Name);

  Result := inherited;

  if Assigned(FOnSubmit) then
    begin
      Result.AddStringParam('method', 'post');
      if FEncType = iwbsfeMultipart then
        Result.AddStringParam('enctype', 'multipart/form-data')
      else if FEncType = iwbsfeText then
        Result.AddStringParam('enctype', 'text/plain');
      Result.AddStringParam('action', IWBSRegisterRestCallBack(AContext.WebApplication, HTMLName+'.FormSubmit', DoSubmit, (FEncType = iwbsfeMultipart)));
    end
  else
    Result.AddStringParam('onSubmit', 'return FormDefaultSubmit();');
end;

end.
