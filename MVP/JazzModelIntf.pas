unit JazzModelIntf;

interface

uses
  JazzCommandSetIntf,
  JazzSelectionIntf;

type
  IModel = interface(IInterface)
    ['{E5BC725A-4DFF-4FDC-A20A-65E1BFCA340C}']
    function GetCommandSet: ICommandSet;
    function GetObjectValue: IInterface;
    function GetSelection: ISelection;
    procedure SetCommandSet(const Value: ICommandSet);
    procedure SetObjectValue(const Value: IInterface);
    property CommandSet: ICommandSet read GetCommandSet write SetCommandSet;
    property Selection: ISelection read GetSelection;
    property ObjectValue: IInterface read GetObjectValue write SetObjectValue;
  end;

  IValueTypeModel = interface(IModel)
    ['{799F5382-BF27-4EAA-B915-E9675F0AE451}']
  end;
  IMemberModel = interface(IValueTypeModel)
    ['{522EF86F-9F67-4658-85A7-DDF1DB9322E6}']
  end;
  IBlobModel = interface(IMemberModel)
    ['{A924164C-06D5-4A1C-B956-ED88503FD0C7}']
  end;
  IBooleanModel = interface(IMemberModel)
    ['{6333D629-2897-484A-9510-D4370FABC2D8}']
  end;
  ICharModel = interface(IMemberModel)
    ['{60338ACE-D904-4825-895B-183CC0B11AED}']
  end;
  ICurrencyModel = interface(IMemberModel)
    ['{4FD28911-AAE5-4743-ADEA-FB03CF0774CC}']
  end;
  IDateModel = interface(IMemberModel)
    ['{229B85A4-0C34-4A64-A316-8C5FBDDA7F3B}']
  end;
  IFloatModel = interface(IMemberModel)
    ['{7C4913AA-6772-4E93-B800-B5EF8BD27150}']
  end;
  IIntegerModel = interface(IMemberModel)
    ['{0DED5361-E429-4046-9D6B-280306F84976}']
  end;
  ILongIntModel = interface(IMemberModel)
    ['{3190D529-0277-4ED0-BF0C-82EAE6F4E933}']
  end;
  IMemoModel = interface(IMemberModel)
    ['{C4965C56-BBA8-484A-990E-57EAE77A8121}']
  end;
  ISmallIntModel = interface(IMemberModel)
    ['{B146E544-FFFB-4B0C-BCD8-92267F2D1C35}']
  end;
  IStringModel = interface(IMemberModel)
    ['{217257C0-AE8F-4D41-BD14-7531DC3E363D}']
  end;
  IWideStringModel = interface(IMemberModel)
    ['{53A96D2B-607D-495B-B780-D9DDFBCE652E}']
  end;
  IObjectModel = interface(IMemberModel)
    ['{6E3CC03B-5909-4BE2-B25B-1B3D630CDAAC}']
  end;
  IObjectListModel = interface(IMemberModel)
    ['{8D3F0E1B-501D-4980-A6E1-6718E1CE08CF}']
  end;

implementation

end.
