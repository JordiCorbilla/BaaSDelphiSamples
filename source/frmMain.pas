// Copyright (c) 2016, Jordi Corbilla
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// - Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// - Neither the name of this library nor the names of its contributors may be
// used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Forms, FMX.Dialogs, FMX.TabControl, System.Actions, FMX.ActnList,
  FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  lib.firebase.rest, DBXJSON, System.JSON, Data.DBXJSONCommon, lib.document, generics.collections,
  FMX.Layouts, FMX.ListBox, FMX.WebBrowser, FMX.Ani;

type
  Tmain = class(TForm)
    ActionList1: TActionList;
    PreviousTabAction1: TPreviousTabAction;
    TitleAction: TControlAction;
    NextTabAction1: TNextTabAction;
    TopToolBar: TToolBar;
    ToolBarLabel: TLabel;
    BottomToolBar: TToolBar;
    Refresh: TAction;
    SpeedButton1: TSpeedButton;
    ListBox1: TListBox;
    SpeedButton2: TSpeedButton;
    Upload: TAction;
    AniIndicator2: TAniIndicator;
    FloatAnimation2: TFloatAnimation;
    OpenDialog1: TOpenDialog;
    procedure RefreshExecute(Sender: TObject);
    procedure ListBoxItem1Click(Sender: TObject);
    procedure UploadExecute(Sender: TObject);
  private
  public
  end;

var
  main: Tmain;

implementation

uses
  StrUtils,

{$IFDEF ANDROID}
   Androidapi.JNI.GraphicsContentViewText,
   Androidapi.Helpers,
   Androidapi.JNI.JavaTypes,
   Androidapi.JNI.Net,
{$ENDIF}
 System.IOUtils, System.Threading;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.iPhone4in.fmx IOS}

procedure Tmain.RefreshExecute(Sender: TObject);
begin
  //Load files from the cloud
  TTask.Create(
      procedure
      var
        return : string;
         List : TList<IDocument>;
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            AniIndicator2.Enabled := true;
            AniIndicator2.Visible := true;
          end
        );

        try
          return := TFirebaseRest.New.GetCollection;
        Finally
          List := TDocumentParser.ParseRequestJSON(return);
          TThread.Synchronize(nil,
            procedure
            var
              i: Integer;
              ListBoxItem : TListBoxItem;
            begin
              ListBox1.BeginUpdate;
              Listbox1.Clear;
              for i := 0 to list.Count-1 do
              begin
                ListBoxItem := TListBoxItem.Create(ListBox1);
                ListBoxItem.Text := list[i].FileName;
                ListBoxItem.Data :=  TDocument(list[i]);
                ListBoxItem.ItemData.Accessory := TListBoxItemData.TAccessory(1);
                ListBoxItem.OnClick := ListBoxItem1Click;
                ListBox1.AddObject(ListBoxItem);
              end;
              ListBox1.EndUpdate;
             end
          );
        end;

        TThread.Synchronize(nil,
          procedure
          begin
            AniIndicator2.Enabled := false;
            AniIndicator2.Visible := false;
           end
        );
      end
    ).Start;
end;

procedure Tmain.UploadExecute(Sender: TObject);
var
  filename : string;
begin
  OpenDialog1.InitialDir := TPath.GetSharedDownloadsPath;

  // Only allow existing files to be selected
  //OpenDialog1.Options := [ofFileMustExist];

  // Allow only .dpr and .pas files to be selected
  OpenDialog1.Filter := 'PDF Files|*.pdf|MS Word|*.docx';

  // Select pascal files as the starting filter type
  //OpenDialog1.FilterIndex := 2;

  // Display the open file dialog
  if OpenDialog1.Execute then
  begin
    filename := OpenDialog1.FileName;

    TTask.Create(
      procedure
      var
        strFileStream: TFileStream;
        arr : TJSONArray;
        s : string;
        ByteArray: array of Byte;
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            AniIndicator2.Enabled := true;
            AniIndicator2.Visible := true;
          end
        );

        try
          strFileStream := TFileStream.Create(filename, fmOpenRead);
          arr := TDBXJSONTools.StreamToJSON(strFileStream, 0, strFileStream.Size);
          strFileStream.Position := 0;
          SetLength(ByteArray, strFileStream.Size);
          strFileStream.Read(ByteArray[0], strFileStream.Size);
          strFileStream.Free;
          s := '{"document":"'+extractfilename(filename)+'","array":'+arr.toJSON+'}';
          TFirebaseRest.New.Add(s);
        Finally
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('File uploaded successfully!');
             end
          );

        end;

        TThread.Synchronize(nil,
          procedure
          begin
            AniIndicator2.Enabled := false;
            AniIndicator2.Visible := false;
           end
        );
      end
    ).Start;
  end
  else
  begin
    ShowMessage('Open file was cancelled');
  end;

  // Free up the dialog
  OpenDialog1.Free;
end;

procedure Tmain.ListBoxItem1Click(Sender: TObject);
var
 fName : String;
 document : TDocument;
{$IFDEF ANDROID}
 Intent : JIntent;
 URI : Jnet_Uri;
{$ENDIF}
begin
  //Save the document to the device
  document := ((Sender as TListBoxItem).Data as TDocument);
  document.Save(TPath.GetSharedDownloadsPath + PathDelim);
  fName := TPath.Combine(TPath.GetSharedDownloadsPath, document.FileName);
  //IF Android, then call the open file
  {$IFDEF ANDROID}
    URI := TJnet_Uri.JavaClass.parse(StringToJString('file:///' + fName));
    intent := TJIntent.Create;
    intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    intent.setDataAndType(URI,StringToJString('application/pdf'));
    SharedActivity.startActivity(intent);
  {$ENDIF}
end;

end.
