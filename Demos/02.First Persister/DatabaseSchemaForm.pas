unit DatabaseSchemaForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ImgList, ToolWin, Menus;

type
  TFormDatabaseSchema = class(TForm)
    ImageListMenu: TImageList;
    ImageListToolBar: TImageList;
    MainMenu: TMainMenu;
    MenuItemClose: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemGenerateScript: TMenuItem;
    MenuItemOptions: TMenuItem;
    MenuItemQuoteIdentifiers: TMenuItem;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    ToolButtonGenerateSchema: TToolButton;
    ToolButtonGenerateScript: TToolButton;
    MemoSchema: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDatabaseSchema: TFormDatabaseSchema;

procedure CreateDatabaseSchemaForm;

implementation

{$R *.dfm}

procedure CreateDatabaseSchemaForm;
begin
  FormDatabaseSchema:= TFormDatabaseSchema.Create(Application);
end;

end.
