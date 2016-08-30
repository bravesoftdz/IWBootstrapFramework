unit IWBSFRegister;

interface
   uses Classes, System.StrUtils, System.SysUtils, DesignEditors,
   IWDsnPaintHandlers, IWBaseControl;

   type
   TIWBSPaintHandlerFSearch = class (TIWPaintHandlerRectangle)
  public
    procedure Paint; override;
  end;

    const
  CNST_DEFAULTFONTNAME = 'Tahoma';
  CNST_GLYPHICONSFONT = 'GLYPHICONS Halflings';
  CNST_PROPORTIONALFONT = 'Courier New';

procedure Register;

implementation
  uses  Windows, Forms, Dialogs, Graphics,
     IWBSControls, IWBSCustomInput,
     IWBSRegion, IWBSInput, IWBSButton, IWBSDropDown,
     IWBSCommon, IWBSImage, IWBSFSearch;

procedure Register;
begin
  RegisterComponents('IW BootsTrap', [TIWBSFSearch]);
end;


{ TIWBSPaintHandlerFSearch }

procedure TIWBSPaintHandlerFSearch.Paint;
var
  LRect, LIcon: TRect;
  s, c: string;
  LMultiLine: boolean;
begin
  LRect := Rect(0, 0, Control.Width, Control.Height);

  ControlCanvas.Brush.Color := clWhite;
  ControlCanvas.Pen.Color := clGray;
  ControlCanvas.Font.Name := CNST_DEFAULTFONTNAME;
  ControlCanvas.Font.Size := 10;
  ControlCanvas.Font.Color := clBlack;
  ControlCanvas.Rectangle(LRect);

  Inc(LRect.Top, 2);
  Inc(LRect.Left, 2);
  Dec(LRect.Bottom, 2);
  Dec(LRect.Right, 2);
  ControlCanvas.Pen.Color := clLtGray;
  ControlCanvas.Rectangle(LRect);

  if Control is TIWBSCustomInput then begin
    LMultiLine := False;

    s := TIWBSCustomInput(Control).DataField;
    if Control is TIWBSCustomTextInput then
      begin
        if s = '' then
          s := TIWBSInput(Control).Text;
        if s = '' then begin
          s := TIWBSInput(Control).PlaceHolder;
          ControlCanvas.Font.Color := clLtGray;
        end;
        if Control is TIWBSMemo then
          LMultiLine := True;
      end;



    Inc(LRect.Top, 1);
    Inc(LRect.Left, 8);
    Dec(LRect.Bottom, 1);
    Dec(LRect.Right, 8);
    if LMultiLine then
      ControlCanvas.TextRect(LRect,s,[])
    else
      DrawTextEx(ControlCanvas.Handle, PChar(s), Length(s), LRect, DT_SINGLELINE+DT_VCENTER, nil);
  end;
end;

initialization
IWRegisterPaintHandler('TIWBSFSearch',TIWBSPaintHandlerFSearch);


finalization
  IWUnRegisterPaintHandler('TIWBSFSearch');

end.
