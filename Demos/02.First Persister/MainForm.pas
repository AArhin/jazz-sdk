unit MainForm;

interface

uses
  Classes, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Menus, Graphics,
  ImgList, ToolWin;

type
  TFormMain = class(TForm)
    ImageListMenu: TImageList;
    ImageListTitle: TImageList;
    ImageListToolBar: TImageList;
    ImageTitle: TImage;
    LabelTitle: TLabel;
    ListViewAddress: TListView;
    ListViewPerson: TListView;
    ListViewPopupMenu: TPopupMenu;
    MainMenu: TMainMenu;
    MemoHistory: TMemo;
    MenuItemClearHistory: TMenuItem;
    MenuItemActiveSession: TMenuItem;
    MenuItemADOMechanism: TMenuItem;
    MenuItemCancelDeleting: TMenuItem;
    MenuItemClearCache: TMenuItem;
    MenuItemClearData: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemDBXMechanism: TMenuItem;
    MenuItemDeleteSelected: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemGenerateSchema: TMenuItem;
    MenuItemLoadData: TMenuItem;
    MenuItemLogHistory: TMenuItem;
    MenuItemN1: TMenuItem;
    MenuItemN2: TMenuItem;
    MenuItemN3: TMenuItem;
    MenuItemN4: TMenuItem;
    MenuItemOptions: TMenuItem;
    MenuItemOpenFile: TMenuItem;
    PageControl: TPageControl;
    PanelAddressList: TPanel;
    PanelPersonList: TPanel;
    PanelTitle: TPanel;
    MenuItemSaveToFile: TMenuItem;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    TabSheetData: TTabSheet;
    TabSheetHelp: TTabSheet;
    TabSheetHistory: TTabSheet;
    ToolBar: TToolBar;
    ToolButtonDelete: TToolButton;
    ToolButtonEdit: TToolButton;
    ToolButtonHelp: TToolButton;
    ToolButtonHistory: TToolButton;
    ToolButtonHome: TToolButton;
    ToolButtonInsert: TToolButton;
    ToolButtonLoadData: TToolButton;
    ToolButtonS1: TToolButton;
    ToolButtonS2: TToolButton;
    ToolButtonS3: TToolButton;
    ToolButtonS4: TToolButton;
    ToolButtonPost: TToolButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

end.

