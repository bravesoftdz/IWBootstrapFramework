unit IWBSItemsEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ValEdit, Buttons, ExtCtrls, DesignIntf, DesignEditors;

type
  TfrmValueListEditor = class(TForm)
    pnlOptions: TPanel;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    vleItems: TValueListEditor;
  end;

  TValueListPropertyEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

var
  frmValueListEditor: TfrmValueListEditor;

implementation
   uses IWBSCustomInput;


{$R *.dfm}

{ TValueListPropertyEditor }

procedure TValueListPropertyEditor.Edit;
var
 frm: TfrmValueListEditor;
 temp: string;
begin
  Temp := TIWBSCustomSelectInput(GetComponent(0)).Items.Text;
  frm := TfrmValueListEditor.Create(nil);
  try
    frm.vleItems.Strings.Text := Temp;
    if frm.ShowModal = mrOk then
    begin
      temp := frm.vleItems.Strings.Text;
      TIWBSCustomSelectInput(GetComponent(0)).Items.Text := Temp;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

function TValueListPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly] - [paSubProperties];
end;

end.