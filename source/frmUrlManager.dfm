object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Url Manager'
  ClientHeight = 526
  ClientWidth = 755
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 27
    Width = 26
    Height = 13
    Caption = 'User:'
  end
  object Label2: TLabel
    Left = 224
    Top = 27
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object Label3: TLabel
    Left = 24
    Top = 59
    Width = 17
    Height = 13
    Caption = 'Url:'
  end
  object ListView1: TListView
    Left = 24
    Top = 164
    Width = 713
    Height = 345
    Columns = <
      item
        Caption = 'User'
        Width = 100
      end
      item
        Caption = 'Password'
        Width = 100
      end
      item
        Caption = 'Url'
        Width = 480
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object edtUser: TEdit
    Left = 56
    Top = 24
    Width = 137
    Height = 21
    TabOrder = 1
  end
  object edtPassword: TEdit
    Left = 280
    Top = 24
    Width = 137
    Height = 21
    TabOrder = 2
  end
  object edtUrl: TMemo
    Left = 56
    Top = 56
    Width = 681
    Height = 65
    TabOrder = 3
  end
  object Button1: TButton
    Left = 662
    Top = 25
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 4
  end
  object Button2: TButton
    Left = 662
    Top = 133
    Width = 75
    Height = 25
    Caption = 'Reload'
    TabOrder = 5
  end
end
