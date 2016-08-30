object frmValueListEditor: TfrmValueListEditor
  Left = 331
  Top = 276
  Caption = 'Value List Property Editor'
  ClientHeight = 302
  ClientWidth = 694
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlOptions: TPanel
    Left = 0
    Top = 261
    Width = 694
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      694
      41)
    object btnCancel: TBitBtn
      Left = 603
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOk: TBitBtn
      Left = 522
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object vleItems: TValueListEditor
    Left = 0
    Top = 0
    Width = 694
    Height = 261
    Align = alClient
    KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
    TabOrder = 1
  end
end
