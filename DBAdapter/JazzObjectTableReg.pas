unit JazzObjectTableReg;

interface

uses
  Classes, Designintf, DesignEditors, DBReg, DSDesign;

type
  TObjectTableEditor = class(TDataSetEditor)
  protected
    function GetDSDesignerClass: TDSDesignerClass; override;
  end;

procedure Register;

implementation

uses
  JazzObjectTable, JazzObjectTableDesign;

procedure Register;
begin
  RegisterComponents('Jazz', [TObjectTable]);
  RegisterComponentEditor(TObjectTable, TObjectTableEditor);
end;

{ TObjectTableEditor }

function TObjectTableEditor.GetDSDesignerClass: TDSDesignerClass;
begin
  Result := TObjectTableDesigner;
end;

end.
