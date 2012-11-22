object frmPrincipal: TfrmPrincipal
  Left = 341
  Top = 151
  BorderWidth = 10
  Caption = 'DEMO 01 - Pessoas'
  ClientHeight = 540
  ClientWidth = 655
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pag: TPageControl
    Left = 0
    Top = 0
    Width = 655
    Height = 540
    ActivePage = tabDados
    Align = alClient
    TabOrder = 0
    TabWidth = 70
    object tabDados: TTabSheet
      BorderWidth = 10
      Caption = 'Dados'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Bevel: TBevel
        Left = 0
        Top = 306
        Width = 627
        Height = 6
        Align = alBottom
        Shape = bsSpacer
        ExplicitTop = 302
      end
      object dbg: TDBGrid
        Left = 0
        Top = 145
        Width = 627
        Height = 161
        Align = alClient
        DataSource = dsPessoa
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 627
        Height = 145
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object DBNavigator: TDBNavigator
          Left = 0
          Top = 113
          Width = 240
          Height = 25
          DataSource = dsPessoa
          TabOrder = 3
        end
        object gbxPesquisa: TGroupBox
          Left = 0
          Top = 2
          Width = 209
          Height = 105
          Caption = ' Pesquisa '
          TabOrder = 0
          object lblPesqNome: TLabel
            Left = 16
            Top = 19
            Width = 89
            Height = 13
            Caption = 'Nome ( inicia com )'
          end
          object edtPesqNome: TEdit
            Left = 16
            Top = 38
            Width = 177
            Height = 21
            TabOrder = 0
          end
          object btnAbrir: TButton
            Left = 118
            Top = 65
            Width = 75
            Height = 25
            Caption = 'Abrir'
            TabOrder = 1
            OnClick = btnAbrirClick
          end
        end
        object gbxLocalizar: TGroupBox
          Left = 215
          Top = 2
          Width = 202
          Height = 105
          Caption = ' Localizar '
          TabOrder = 1
          object lblLocNome: TLabel
            Left = 16
            Top = 19
            Width = 27
            Height = 13
            Caption = 'Nome'
          end
          object edtLocNome: TEdit
            Left = 16
            Top = 38
            Width = 169
            Height = 21
            Enabled = False
            TabOrder = 0
          end
          object btnLocalizar: TButton
            Left = 110
            Top = 65
            Width = 75
            Height = 25
            Caption = 'Localizar'
            Enabled = False
            TabOrder = 1
            OnClick = btnLocalizarClick
          end
        end
        object gbxFiltro: TGroupBox
          Left = 423
          Top = 2
          Width = 202
          Height = 105
          Caption = ' Localizar '
          TabOrder = 2
          object lblFiltroNome: TLabel
            Left = 16
            Top = 19
            Width = 72
            Height = 13
            Caption = 'Tipo de Pessoa'
          end
          object btnFiltrar: TButton
            Left = 110
            Top = 65
            Width = 75
            Height = 25
            Caption = 'Filtrar'
            Enabled = False
            TabOrder = 1
            OnClick = btnFiltrarClick
          end
          object cbxTipoPessoa: TComboBox
            Left = 16
            Top = 38
            Width = 169
            Height = 21
            Style = csDropDownList
            Enabled = False
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 0
            Text = 'Pessoa F'#237'sica'
            OnChange = cbxTipoPessoaChange
            Items.Strings = (
              'Pessoa F'#237'sica'
              'Pessoa Jur'#237'dica')
          end
        end
        object btnApplyChanges: TButton
          Left = 246
          Top = 113
          Width = 115
          Height = 25
          Caption = 'Salvar altera'#231#245'es'
          Enabled = False
          TabOrder = 4
          OnClick = btnApplyChangesClick
        end
        object btnCancelChanges: TButton
          Left = 367
          Top = 113
          Width = 115
          Height = 25
          Caption = 'Cancelar atera'#231#245'es'
          Enabled = False
          TabOrder = 5
          OnClick = btnCancelChangesClick
        end
      end
      object TPanel
        Left = 0
        Top = 312
        Width = 627
        Height = 180
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object DBMemo: TDBMemo
          Left = 0
          Top = 0
          Width = 468
          Height = 180
          Align = alClient
          DataField = 'Comentario'
          DataSource = dsPessoa
          TabOrder = 0
        end
        object Panel2: TPanel
          Left = 468
          Top = 0
          Width = 159
          Height = 180
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object DBImage: TDBImage
            Left = 6
            Top = 0
            Width = 153
            Height = 149
            DataField = 'Foto'
            DataSource = dsPessoa
            Stretch = True
            TabOrder = 0
          end
          object btnImgAdiciona: TButton
            Left = 5
            Top = 155
            Width = 74
            Height = 25
            Caption = 'Adiciona'
            Enabled = False
            TabOrder = 1
            OnClick = btnImgAdicionaClick
          end
          object btnImgRemover: TButton
            Left = 84
            Top = 155
            Width = 74
            Height = 25
            Caption = 'Remove'
            Enabled = False
            TabOrder = 2
            OnClick = btnImgRemoverClick
          end
        end
      end
    end
    object tabSQL: TTabSheet
      BorderWidth = 10
      Caption = 'Log SQL'
      ImageIndex = 1
      object mmoSQL: TMemo
        Left = 0
        Top = 0
        Width = 627
        Height = 492
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Bitstream Vera Sans Mono'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object tblPessoa: TObjectTable
    AfterApplyChanges = AfterChanges
    AfterCancelChanges = AfterChanges
    AfterClose = tblPessoaAfterClose
    AfterDelete = tblPessoaAfterPost
    AfterPost = tblPessoaAfterPost
    OnFilterRecord = tblPessoaFilterRecord
    OnNewRecord = tblPessoaNewRecord
    ObjectClassName = 'TPessoa'
    Left = 40
    Top = 192
    object tblPessoaCodigoPessoa: TIntegerField
      DisplayLabel = 'C'#243'digo'
      FieldName = 'CodigoPessoa'
    end
    object tblPessoaNome: TStringField
      DisplayWidth = 70
      FieldName = 'Nome'
      Size = 100
    end
    object tblPessoaTipoPessoa: TStringField
      DisplayLabel = 'Tipo'
      FieldName = 'TipoPessoa'
      Size = 1
    end
    object tblPessoaComentario: TMemoField
      FieldName = 'Comentario'
      Visible = False
      BlobType = ftMemo
    end
    object tblPessoaFoto: TGraphicField
      FieldName = 'Foto'
      Visible = False
      BlobType = ftGraphic
    end
    object tblPessoaDataCadastro: TDateTimeField
      DisplayLabel = 'Data de Cadastro'
      FieldName = 'DataCadastro'
    end
  end
  object dsPessoa: TDataSource
    DataSet = tblPessoa
    OnStateChange = dsPessoaStateChange
    Left = 40
    Top = 224
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.bmp;*.ico;*.emf;*.wmf)|*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.w' +
      'mf|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg' +
      '|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanced Metafiles (*' +
      '.emf)|*.emf|Metafiles (*.wmf)|*.wmf'
    Left = 80
    Top = 224
  end
end
