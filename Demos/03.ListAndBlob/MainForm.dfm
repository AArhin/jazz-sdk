object Form1: TForm1
  Left = 431
  Top = 214
  Caption = 'Form1'
  ClientHeight = 428
  ClientWidth = 598
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 41
    Width = 598
    Height = 387
    ActivePage = TabSheetMemoList
    Align = alClient
    TabOrder = 0
    object TabSheetSQL: TTabSheet
      Caption = 'TabSheetSQL'
      object MemoSQL: TMemo
        Left = 0
        Top = 0
        Width = 590
        Height = 359
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object TabSheetListView: TTabSheet
      Caption = 'TabSheetListView'
      ImageIndex = 1
      object Splitter: TSplitter
        Left = 0
        Top = 251
        Width = 590
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 258
      end
      object ListViewEnderecos: TListView
        Left = 0
        Top = 254
        Width = 590
        Height = 105
        Align = alBottom
        Columns = <
          item
            Caption = 'ID'
            Width = 250
          end
          item
            Caption = 'Rua'
            Width = 150
          end
          item
            Caption = 'Numero'
            Width = 150
          end
          item
            Caption = 'Bairro'
            Width = 100
          end
          item
            Caption = 'Cidade'
            Width = 100
          end
          item
            Caption = 'UF'
          end
          item
            Caption = 'ID Owner'
            Width = 250
          end>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Bitstream Vera Sans Mono'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        ViewStyle = vsReport
      end
      object PanelList: TPanel
        Left = 0
        Top = 0
        Width = 590
        Height = 251
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object PanelFoto: TPanel
          Left = 405
          Top = 0
          Width = 185
          Height = 251
          Align = alRight
          BevelOuter = bvLowered
          BorderWidth = 1
          TabOrder = 0
          DesignSize = (
            185
            251)
          object EditImage: TImage
            Left = 5
            Top = 32
            Width = 175
            Height = 175
            AutoSize = True
          end
          object ButtonAddFoto: TButton
            Left = 5
            Top = 4
            Width = 75
            Height = 25
            Caption = 'Add Foto'
            TabOrder = 0
            OnClick = ButtonAddFotoClick
          end
          object ButtonRemoveFoto: TButton
            Left = 84
            Top = 4
            Width = 75
            Height = 25
            Caption = 'Remove Foto'
            TabOrder = 1
            OnClick = ButtonRemoveFotoClick
          end
          object CheckBoxAjustarFoto: TCheckBox
            Left = 6
            Top = 229
            Width = 139
            Height = 17
            Anchors = [akLeft, akBottom]
            Caption = 'Ajustar foto'
            Color = clBtnFace
            ParentColor = False
            TabOrder = 2
            OnClick = CheckBoxAjustarFotoClick
          end
        end
        object ListViewPessoa: TListView
          Left = 0
          Top = 0
          Width = 405
          Height = 251
          Align = alClient
          Columns = <
            item
              Caption = 'ID'
              Width = 250
            end
            item
              Caption = 'Nome'
              Width = 150
            end
            item
              Caption = 'Data de Nascimento'
              Width = 150
            end>
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Bitstream Vera Sans Mono'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          ViewStyle = vsReport
        end
      end
    end
    object TabSheetMemoList: TTabSheet
      Caption = 'TabSheetMemoList'
      ImageIndex = 2
      object MemoList: TMemo
        Left = 0
        Top = 0
        Width = 590
        Height = 359
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object PanelTools: TPanel
    Left = 0
    Top = 0
    Width = 598
    Height = 41
    Align = alTop
    TabOrder = 1
    object ButtonAdd: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 0
      OnClick = ButtonAddClick
    end
    object EditAdd: TEdit
      Left = 96
      Top = 10
      Width = 89
      Height = 21
      TabOrder = 1
      Text = '100'
    end
    object ButtonLoad: TButton
      Left = 272
      Top = 8
      Width = 113
      Height = 25
      Caption = 'Populate Memo'
      TabOrder = 2
      OnClick = ButtonLoadClick
    end
    object ButtonLoadList: TButton
      Left = 192
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Load List'
      TabOrder = 3
      OnClick = ButtonLoadListClick
    end
    object ButtonDeleteAll: TButton
      Left = 519
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Delete All'
      TabOrder = 4
      OnClick = ButtonDeleteAllClick
    end
    object ButtonImporty: TButton
      Left = 391
      Top = 8
      Width = 90
      Height = 25
      Caption = 'Import DB Meta'
      TabOrder = 5
      OnClick = ButtonImportyClick
    end
  end
  object PictureDialog: TOpenPictureDialog
    Left = 452
    Top = 105
  end
  object XPManifest1: TXPManifest
    Left = 20
    Top = 121
  end
  object IBDatabase1: TIBDatabase
    DatabaseName = 
      'C:\Projetos\Jazz\Sources\Community\Demos\00.Common\DB\LISTANDBLO' +
      'B.FDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    AllowStreamedConnected = False
    Left = 16
    Top = 160
  end
end
