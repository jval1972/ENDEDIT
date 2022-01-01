//------------------------------------------------------------------------------
//
//  ENDEDIT: An ENDTEXT Editor
//  Copyright (C) 2021-2022 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Choose image conversion form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/endedit/
//------------------------------------------------------------------------------

unit frm_imgconvertmethod;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ee_textart;

type
  TImgConvertMethodForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel3: TPanel;
    RadioGroup1: TRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ChooseImgConvertMethod(var m: textartmethod_t): boolean;

implementation

{$R *.dfm}

function ChooseImgConvertMethod(var m: textartmethod_t): boolean;
var
  f: TImgConvertMethodForm;
  i: integer;
begin
  Result := False;
  f := TImgConvertMethodForm.Create(nil);
  try
    f.RadioGroup1.Items.Clear;
    for i := 0 to Ord(NUMTEXTARTMETHOD) - 1 do
      f.RadioGroup1.Items.Add(TEXTMETHODNAMES[i]);
    f.RadioGroup1.ItemIndex := Ord(m);
    f.ShowModal;
    if f.ModalResult = mrOK then
    begin
      m := textartmethod_t(f.RadioGroup1.ItemIndex);
      Result := True;
    end;
  finally
    f.Free;
  end;
end;

end.
