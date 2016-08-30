unit IWBSRepeater;

interface

uses Classes, IWBSRegion, Db, IWBaseRenderContext, IWBSCustomControl;

type
  TIWBSRepeater = class(TIWBSRegion)
  private
    FDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure UpdateDataSource;
  protected
     procedure RenderComponents(AContainerContext: TIWContainerContext; APageContext: TIWBasePageContext); override;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property DataSource: TDataSource read FDataSource write SetDataSource;
  end;

implementation

uses
  SysUtils, System.TypInfo;

{ TIWBSRepeater }

constructor TIWBSRepeater.Create(AOwner: TComponent);
begin
  inherited;
  FDataSource := nil;
end;

procedure TIWBSRepeater.RenderComponents(AContainerContext: TIWContainerContext;
  APageContext: TIWBasePageContext);
var
  OldRow: Integer;
  I: Integer;
begin
  if FDataSource <> nil then
    if FDataSource.DataSet <> nil then
      begin
          OldRow:= FDataSource.DataSet.RecNo;
          FDataSource.DataSet.DisableControls;
          try
            FDataSource.DataSet.First;
            while not  FDataSource.DataSet.Eof do
              begin
                try
                   for I := 0 to ComponentCount -1 do
                     begin
                       Components[i].Name:= Component[i].ClassName + IntToStr(FDataSource.DataSet.RecNo);
                     end;

                   inherited;
                finally
                  FDataSource.DataSet.Next;
                end;
              end;
          finally
            FDataSource.DataSet.RecNo:= OldRow;
            FDataSource.DataSet.EnableControls;
          end;
      end;
end;

procedure TIWBSRepeater.SetDataSource(const Value: TDataSource);
begin
  if Value <> FDataSource then
    begin
      if Value = nil then
        begin
          FDataSource:=nil;
          //FreeAndNil(FDataSource);
        end
      else
        begin
         // if FDataSource = nil then
          //  FDataSource := TDataSource.Create(Self);
          //FDataSource.DataSet:= Value.DataSet;
          FDataSource:= Value;
        end;
      if not IsDesignMode then
        UpdateDataSource;
      Invalidate;
    end;
end;

procedure TIWBSRepeater.UpdateDataSource;
var
  I: Integer;
begin
  for I := 0 to ComponentCount -1 do
    begin
      if ISpublishedProp(Components[I], 'DataSource') then
        TIWBSCustomDbControl(Components[I]).DataSource:=  FDataSource;
        //SetPropValue(Components[I], 'DataSource', FDataSource);
    end;
end;

end.
