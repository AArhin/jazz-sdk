unit JazzVCLInteractor;

interface

uses
  JazzInteractor;

type
  TApplicationTerminateInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TFormCloseInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

implementation

uses
  JazzPresenterIntf,
  Forms;

{ TApplicationTerminateInteractor }

procedure TApplicationTerminateInteractor.Execute(Sender: TObject);
begin
  inherited;
  Application.Terminate;
end;

{ TApplicationTerminateInteractor }

procedure TFormCloseInteractor.Execute(Sender: TObject);
begin
  inherited;
  ((Owner as IPresenter).View.Control as TForm).Close; 
end;



end.
