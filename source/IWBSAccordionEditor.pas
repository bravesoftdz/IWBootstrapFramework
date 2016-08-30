unit IWBSAccordionEditor;

interface
  uses
     DesignEditors, DesignIntf, Forms, IWBSAccordion;

  type

 TIWBSAccordionEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


implementation

{ TCSDBGridEditor }

procedure TIWBSAccordionEditor.ExecuteVerb(Index: Integer);
var
  Item:TIWBSAccordionItem;
begin
  inherited;
  case Index of
    0:  begin
          Item:= TIWBSAccordionItem(Designer.CreateChild(TIWBSAccordionItem, Component));
          TIWBSAccordion(Component).InsertItem(Item);
         // Item:= TIWBSAccordion(Component).CreateItem(Component);
         // Item.Name:= Designer.UniqueName(Item.ClassName);
          //TIWBSAccordion(Component).CreateItem(Component).Name:= Designer.UniqueName(Item.ClassName);;
        end;
    1:  Application.MessageBox(PChar('    TSDDBGrid' + #13#10#13#10 + '版本: 6.0'
      + ' (编译版本: 6.0.6.12)' + #13#10#13#10 + ''),
      'About ...', 64);
  end;
end;

function TIWBSAccordionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Add Item';
    1:  Result := 'Remove Item';

  end;
end;

function TIWBSAccordionEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
