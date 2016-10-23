object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 657
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 659
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Load Firebase'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 45
    Top = 80
    Width = 689
    Height = 553
    Lines.Strings = (
      '')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Edit1: TEdit
    Left = 45
    Top = 53
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 172
    Top = 53
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Edit2'
  end
  object Button2: TButton
    Left = 299
    Top = 49
    Width = 110
    Height = 25
    Caption = 'Add To Firebase'
    TabOrder = 4
    OnClick = Button2Click
  end
  object AddFile: TButton
    Left = 467
    Top = 49
    Width = 110
    Height = 25
    Caption = 'Add File To Firebase'
    TabOrder = 5
    OnClick = AddFileClick
  end
  object OpenDialog1: TOpenDialog
    Left = 600
    Top = 8
  end
end
