unit JazzVCLPresenterIntf;

interface

uses
  JazzPresenterIntf;

type
  ISimpleControlPresenter = interface(IMemberPresenter)
    ['{B991247F-0A56-4862-8B8F-E37AC5D83CAC}']
  end;
  
  IObjectControlPresenter = interface(IObjectPresenter)
    ['{AF8A10AE-DF80-4091-AF6B-021289309C05}']
  end;
  
  IListControlPresenter = interface(IObjectListPresenter)
    ['{6A17B85B-79CB-4CCC-ADFC-0848FC2A9525}']
  end;

  IListViewPresenter = interface(ICompositePresenter)
    ['{5160B0FA-C0ED-4571-B645-BF1CAB2BA64C}']
    function Add(const Column: TObject; const MemberName: string): IValueTypePresenter; overload;
  end;

  ITreeViewPresenter = interface(ICompositePresenter)
    ['{A61D86E3-2A38-4CFB-8412-A528FF7D957D}']
  end;

implementation

end.
