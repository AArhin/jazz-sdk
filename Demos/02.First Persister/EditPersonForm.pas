unit EditPersonForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, ComCtrls, Buttons;

type
  TFormEditPerson = class(TForm)
    BevelBottom: TBevel;
    ButtonApply: TButton;
    ButtonCancel: TButton;
    ButtonNew: TButton;
    ButtonOK: TButton;
    EditBirthDate: TDateTimePicker;
    EditCredit: TEdit;
    EditDocument: TMaskEdit;
    EditID: TEdit;
    EditName: TEdit;
    EditPicture: TImage;
    GroupBoxPicture: TGroupBox;
    LabelAddress: TLabel;
    LabelBirthDate: TLabel;
    LabelCredit: TLabel;
    LabelDocument: TLabel;
    LabelID: TLabel;
    LabelLastUpdate: TLabel;
    LabelName: TLabel;
    ListBoxAddress: TListBox;
    PanelBottom: TPanel;
    PanelImage: TPanel;
    SpeedButtonClear: TSpeedButton;
    SpeedButtonLoad: TSpeedButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEditPerson: TFormEditPerson;

procedure CreateEditPersonForm;

implementation

{$R *.dfm}

procedure CreateEditPersonForm;
begin
  FormEditPerson:= TFormEditPerson.Create(Application);
end;

end.
