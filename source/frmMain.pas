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
    procedure RefreshExecute(Sender: TObject);
    procedure ListBoxItem1Click(Sender: TObject);
    procedure UploadExecute(Sender: TObject);
  private
    function LoadDocuments(jsonString: string): TList<IDocument>;
    { Private declarations }
  public
    { Public declarations }
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
 System.IOUtils, System.Threading, lib.runThread;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.iPhone4in.fmx IOS}

procedure Tmain.RefreshExecute(Sender: TObject);
var
  firebase : string;
  List : TList<IDocument>;
  i: Integer;
  ListBoxItem : TListBoxItem;
  aTask: array of ITask;
begin
  //Load files from the cloud
//  firebase := TFirebaseRest.New.GetCollection;
  Setlength (aTask ,1);
  aTask[0] := TTask.Create(
      procedure
      var
        return : string;
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

        end;

        TThread.Synchronize(nil,
          procedure
          begin
            firebase := return;
            AniIndicator2.Enabled := false;
            AniIndicator2.Visible := false;
           end
        );
      end
    );

  aTask[0].Start;
  TTask.WaitForAll(aTask);
//  TTask.WaitForAll(aTask);
  List := LoadDocuments(firebase);
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
end;

procedure Tmain.UploadExecute(Sender: TObject);
begin
//
end;

procedure Tmain.ListBoxItem1Click(Sender: TObject);
var
   fName       : String;
   document : TDocument;
{$IFDEF ANDROID}
   Intent      : JIntent;
   URI         : Jnet_Uri;
{$ENDIF}
begin
  document := ((Sender as TListBoxItem).Data as TDocument);
  document.Save(TPath.GetSharedDownloadsPath + PathDelim);
  fName := TPath.Combine(TPath.GetSharedDownloadsPath, document.FileName);
  {$IFDEF ANDROID}
    URI := TJnet_Uri.JavaClass.parse(StringToJString('file:///' + fName));
    intent := TJIntent.Create;
    intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    intent.setDataAndType(URI,StringToJString('application/pdf'));
    SharedActivity.startActivity(intent);
  {$ENDIF}
end;

function Tmain.LoadDocuments(jsonString : string) : TList<IDocument>;
var
  s : string;
  i : integer;
  j, k : integer;
  doc : string;
  name : string;
  list : TList<IDocument>;
begin
  list := TList<IDocument>.Create;
  s := jsonString;
  i := 1;
  while i > 0 do
  begin
    i := AnsiPos('array', s);
    j := AnsiPos('document', s);
    if ((i > 0) and (j>0)) then
    begin
      doc := copy(s, i, j-i);
      doc := doc.Replace('array":', '');
      doc := doc.Replace(',"', '');
      name := AnsiRightStr(s, length(s)-j+1);
      k := AnsiPos('}', name);
      name := copy(name, 0, k);
      name := name.Replace('document":"', '');
      name := name.Replace('"}', '');
      s := AnsiRightStr(s, length(s)-(j+k));
      list.Add(TDocument.New(name, doc));
    end;
  end;
  result := list;
end;

end.
