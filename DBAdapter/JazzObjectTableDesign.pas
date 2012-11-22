unit JazzObjectTableDesign;

{$I Jazz.inc}

interface

uses
{$IFNDEF DELPHI6}
  ActnPopup,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DSDesign, Menus, DB, StdCtrls, DBCtrls, ExtCtrls,
  JazzObjectTable, DesignIntf;

type
  TObjectTableEditor = class(TFieldsEditor)
  private
    function GetObjectTable: TObjectTable;
    procedure SetObjectTable(const Value: TObjectTable);
//    procedure DoImport(Sender: TObject);
  protected
    procedure ConfigureMenu;
  public
    property DataSet: TObjectTable read GetObjectTable write SetObjectTable;
  end;

  TObjectTableDesigner = class(TDSDesigner)
  public
    procedure UpdateMenus(Menu: TPopupMenu; EditState: TEditState); override;
  end;

implementation

{$R *.dfm}

{ TObjectTableEditor }

procedure TObjectTableEditor.ConfigureMenu;
begin
  AddItem.Enabled := False;
  AddItem.Visible := False;

//  Addallfields1.Caption := '&Import...';
//  Addallfields1.OnClick := DoImport;
end;

//procedure TObjectTableEditor.DoImport(Sender: TObject);
//begin
//  ShowMessage('Não implementado');
//end;

function TObjectTableEditor.GetObjectTable: TObjectTable;
begin
  Result := TObjectTable(inherited DataSet);
end;

procedure TObjectTableEditor.SetObjectTable(const Value: TObjectTable);
begin
  inherited DataSet := Value;
end;

{ TObjectTableDesigner }

procedure TObjectTableDesigner.UpdateMenus(Menu: TPopupMenu;
  EditState: TEditState);
begin
  TObjectTableEditor(FieldsEditor).ConfigureMenu;
end;

end.
