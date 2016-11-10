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

unit lib.document;

interface

type
  IDocument = interface
    procedure SetDocument(const Value: string);
    procedure SetFileName(const Value: string);
    function GetDocument() : string;
    function GetFileName() : string;
    property FileName : string read GetFileName write SetFileName;
    property Document : string read GetDocument write SetDocument;
  end;

  TDocument = class(TInterfacedObject, IDocument)
  private
    FFileName: string;
    FDocument: string;
    procedure SetDocument(const Value: string);
    procedure SetFileName(const Value: string);
    function GetDocument() : string;
    function GetFileName() : string;
  public
    property FileName : string read GetFileName write SetFileName;
    property Document : string read GetDocument write SetDocument;
    constructor Create(FileName : string; Document : string);
    class function New(FileName : string; Document : string): IDocument;
  end;

implementation

{ TDocument }

constructor TDocument.Create(FileName, Document: string);
begin
  SetFileName(FileName);
  SetDocument(Document);
end;

function TDocument.GetDocument: string;
begin
  result := FDocument;
end;

function TDocument.GetFileName: string;
begin
  result := FFileName;
end;

class function TDocument.New(FileName : string; Document : string): IDocument;
begin
  result := Create(FileName, Document);
end;

procedure TDocument.SetDocument(const Value: string);
begin
  FDocument := Value;
end;

procedure TDocument.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

end.
