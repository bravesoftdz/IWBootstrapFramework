unit IWBSAccordion;

interface
  uses   System.Classes, Controls, IWBSRegion, IWBSCustomControl,
  IWHTMLTag, IWRenderContext, IWBaseRenderContext, IW.Common.RenderStream,
  IWMarkupLanguageTag, IWBSRegionCommon, IWBaseInterfaces, System.StrUtils;

  type

  TIWBSAccordion = class;

  TIWBSAccordionItem  = class(TIWBSRegion)
  private
    FAccordion: TIWBSAccordion;
    FCaption: string;
    FID: Integer;
    FHTMLResult:TIWHTMLTag;
    procedure SetAccordion(const Value: TIWBSAccordion);
    procedure SetCaption(const Value: string);
    procedure SetID(const Value: Integer);
    protected
      function RenderHTML(AContext: TIWCompContext): TIWHTMLTag; override;
      procedure RenderComponents(AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

 published
    property Accordion:TIWBSAccordion read FAccordion write SetAccordion;
    property Caption:string read FCaption write SetCaption;
    property ID:Integer read FID write SetID;
  end;

  TIWBSAccordionItems = class(TCollection)

  end;



  TIWBSAccordion = class(TIWBSRegion)
  private
    FItems: TList;
    FItemsPadding: string;
    procedure SetItemsPadding(const Value: string);
   protected
      function RenderHTML(AContext: TIWCompContext): TIWHTMLTag; override;
    public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateItem(AOwner: TComponent): TIWBSAccordionItem;
    procedure InsertItem(Item: TIWBSAccordionItem);
    procedure RemoveItem(Item: TIWBSAccordionItem);

  published
    property ItemsPadding:string read FItemsPadding write SetItemsPadding;
  end;


implementation

uses
  System.SysUtils;

{ TIWBSAccordion }

constructor TIWBSAccordion.Create(AOwner: TComponent);
begin
  inherited;
  FItems:= TList.Create;
end;

function TIWBSAccordion.CreateItem(AOwner: TComponent): TIWBSAccordionItem;
begin
  Result := TIWBSAccordionItem.Create(AOwner);
  Result.Parent := Self;
  Result.Accordion := Self;
end;

destructor TIWBSAccordion.Destroy;
begin
  inherited;
end;

procedure TIWBSAccordion.InsertItem(Item: TIWBSAccordionItem);
begin
  FItems.Add(Item);
  Item.FAccordion := Self;
  if FItems.Count = 1 then
    Item.Top := 0
  else
    Item.Top := VertScrollBar.Range;
  Item.Left := 0;
  Item.Align :=  alTop;
  if not (csLoading in ComponentState) then
    AutoScrollInView(Item);
end;

procedure TIWBSAccordion.RemoveItem(Item: TIWBSAccordionItem);
begin
  Item.FAccordion := nil;
  FItems.Remove(Item);
  RemoveControl(Item);
end;



function TIWBSAccordion.RenderHTML(AContext: TIWCompContext): TIWHTMLTag;
begin
  Result:= inherited;
  Result.AddClassParam('panel-group');
end;

procedure TIWBSAccordion.SetItemsPadding(const Value: string);
begin
  if FItemsPadding <> Value then
    begin
      if (Value <> '')
      and ( (Copy(Value, Length(Value) -1, 2) <> 'px')
          and (Copy(Value, Length(Value), 1) <> '%')) then
        raise Exception.Create('ItemsPadding must be of the format: 1px or 1%');
      FItemsPadding := Value;
    end;
end;

{ TIBSAccordionItem }



constructor TIWBSAccordionItem.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TIWBSAccordionItem.Destroy;
begin

  inherited;
end;

procedure TIWBSAccordionItem.RenderComponents(
  AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext);
var
  aComponents:string;
  i:Integer;
  aBody:TIWHTMLTag;
  aHeader:TIWHTMLTag;
begin
  //Render children componentes inside Item
  inherited;
  //Extract children  components of Item
  for i := 0 to  FHTMLResult.Contents.Count -1 do
    begin
      aComponents:= aComponents + FHTMLResult.Contents.Items[i].Render;
    end;
  //Clear Item
  FHTMLResult.Contents.Clear;
  //Add class css to Item
  FHTMLResult.AddClassParam('panel-default');
  //Create the Header
  aHeader:= TIWHTMLTag.CreateTag('div');
  aHeader.AddClassParam('panel-heading');
  aHeader.Contents.AddText('<h4 class="panel-title">' +
                              '<a style="display:block;width:100%;" data-toggle="collapse" data-parent="#' + FAccordion.HTMLName
                                    + '" href="#collapse' + HTMLName + '">' + FCaption +
                              '</a>' +
                           '</h4>');
  //Add the Header to Item
  FHTMLResult.Contents.AddTagAsObject(aHeader);
  //Create the Body
  aBody:= TIWHTMLTag.CreateTag('div');
  aBody.AddClassParam('panel-collapse');
  aBody.AddClassParam('collapse');
  aBody.AddStringParam('id', 'collapse' + HTMLName);
  if FAccordion.ItemsPadding <> '' then
    aBody.AddStringParam('padding', FAccordion.ItemsPadding);
  if FAccordion = nil then
    raise Exception.Create('Accordion Property for Item ' + Self.Name + ' is not set.');

  aBody.Contents.AddText({'<div id="collapse' + HTMLName + '" class="panel-collapse collapse">' + }
                            '<div class="panel-body"'
                            + ifthen(FAccordion.ItemsPadding <> '', ' style="padding:' + FAccordion.ItemsPadding + ';"','' ) + '>' +
                                aComponents + {<-- Add Components to Body}
                            '</div>'{ +
                        '</div>'});
  //Add Body(and components) to Item
  FHTMLResult.Contents.AddTagAsObject(aBody);
end;

function TIWBSAccordionItem.RenderHTML(AContext: TIWCompContext): TIWHTMLTag;
var
  aTAG:TIWHTMLTag;
  aHeader:string;
  aBody:string;
begin
  Result:= inherited;
  //Save Html reference to use in "RenderComponents"
  FHTMLResult:= Result;
end;

procedure TIWBSAccordionItem.SetAccordion(const Value: TIWBSAccordion);
var
  LRecreating: Boolean;
begin
  if FAccordion <> Value then
  begin
    LRecreating := False;
    if not (csLoading in ComponentState) then
    begin
      LRecreating := csRecreating in ControlState;
      if not LRecreating then
        UpdateRecreatingFlag(True);
    end;

    try
      if FAccordion <> nil then
        FAccordion.RemoveItem(Self);
      Parent := Value;
      if Value <> nil then
      begin
        Value.InsertItem(Self);
        if not (csLoading in ComponentState) and not LRecreating then
          RecreateWnd;
      end;
    finally
      if not (csLoading in ComponentState) and not LRecreating then
        UpdateRecreatingFlag(False);
    end;
  end;
end;

procedure TIWBSAccordionItem.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TIWBSAccordionItem.SetID(const Value: Integer);
begin
  FID := Value;
end;

initialization
    RegisterClass(TIWBSAccordionItem);

end.
