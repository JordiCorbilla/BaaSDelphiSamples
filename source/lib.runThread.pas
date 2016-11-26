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

unit lib.runThread;

interface

uses
  System.Classes, REST.Client, Generics.collections, FMX.StdCtrls, FMX.Objects;

type
  TRunThread = class(TThread)
  private
    FListNodes : TList<TNode>;
    FRequest: TRESTRequest;
    FResponse: TRESTResponse;
    FAniIndicator: TAniIndicator;
  public
    constructor Create(request: TRESTRequest; response: TRESTResponse; nodes : TList<TNode>; AniIndicator: TAniIndicator); reintroduce;
  protected
    procedure Execute; override;
  end;

implementation

uses
  System.JSON, System.SysUtils, System.Net.HttpClient;

constructor TRunThread.Create(request: TRESTRequest; response: TRESTResponse; nodes : TList<TNode>; AniIndicator: TAniIndicator);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FListNodes := nodes;
  FRequest := request;
  FResponse := response;
  FAniIndicator := AniIndicator;
end;

procedure TRunThread.Execute;
var
  node : TNode;
  jValue: TJSONValue;
  LJsonArr   : TJSONArray;
  LJsonValue : TJSONValue;
  LItem : TJSONValue;
  status : string;
begin
  Synchronize(
    procedure ()
    begin
        FAniIndicator.Visible := true;
        FAniIndicator.Enabled := true;
    end
  );

  for node in FListNodes do
  begin
    Synchronize(
      procedure ()
      begin
          node.labelNode.Text := 'Checking';
          node.RoundRect.Fill.Color := 4294934352;
      end
    );
    FRequest.Resource := node.Action;
    try
      FRequest.Execute;
      jValue := FResponse.JSONValue;
      LJsonArr := TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(jValue.ToString),0) as TJSONArray;
      for LJsonValue in LJsonArr do
      begin
        for LItem in TJSONArray(LJsonValue) do
        begin
          if (TJSONPair(LItem).JsonString.Value = 'host') then
          begin
            status := TJSONPair(LItem).JsonValue.Value;
            Synchronize(
              procedure ()
              begin
                if status.Equals('alive') then
                begin
                  node.labelNode.Text := 'Alive';
                  node.RoundRect.Fill.Color := node.ColorOn;
                end
                else
                begin
                  node.labelNode.Text := 'Dead';
                  node.RoundRect.Fill.Color := node.ColorOff;
                end;
              end
            );
          end;
        end;
      end;
    except
      on e : Exception do
      begin
        Synchronize(
          procedure ()
          begin
              FAniIndicator.Visible := false;
              FAniIndicator.Enabled := false;
          end
        );
      end;
    end;
  end;

  Synchronize(
    procedure ()
    begin
        FAniIndicator.Visible := false;
        FAniIndicator.Enabled := false;
    end
  );
end;

end.
