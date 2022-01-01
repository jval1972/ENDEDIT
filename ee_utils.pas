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
//  Utility functions
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/endedit/
//------------------------------------------------------------------------------

unit ee_utils;

interface

type
  TLongWordArray = array[0..$FFF] of LongWord;
  PLongWordArray = ^TLongWordArray;

function GetIntInRange(const x: Integer; const amin, amax: Integer): Integer;

function IsIntInRange(const x: Integer; const amin, amax: Integer): Boolean;

function I_VersionBuilt(fname: string = ''): string;

function MinI(const a, b: Integer): Integer;

function MaxI(const a, b: Integer): Integer;

function CopyFile(const sname, dname: string): boolean;

procedure BackupFile(const fname: string);

function MkShortName(const fname: string): string;

procedure I_GoToWebPage(const cmd: string);

implementation

uses
  Windows,
  SysUtils;

function GetIntInRange(const x: Integer; const amin, amax: Integer): Integer;
begin
  Result := x;
  if Result < amin then
    Result := amin
  else if Result > amax then
    Result := amax;
end;

function IsIntInRange(const x: Integer; const amin, amax: Integer): Boolean;
begin
  Result := (x >= amin) and (x <= amax);
end;

function I_VersionBuilt(fname: string = ''): string;
var
  vsize: LongWord;
  zero: LongWord;
  buffer: PByteArray;
  res: pointer;
  len: LongWord;
  i: integer;
begin
  if fname = '' then
    fname := ParamStr(0);
  vsize := GetFileVersionInfoSize(PChar(fname), zero);
  if vsize = 0 then
  begin
    result := '';
    exit;
  end;

  GetMem(buffer, vsize + 1);
  GetFileVersionInfo(PChar(fname), 0, vsize, buffer);
  VerQueryValue(buffer, '\StringFileInfo\040904E4\FileVersion', res, len);
  result := '';
  for i := 0 to len - 1 do
  begin
    if PChar(res)^ = #0 then
      break;
    result := result + PChar(res)^;
    res := pointer(integer(res) + 1);
  end;
  FreeMem(pointer(buffer), vsize + 1);
end;

function MinI(const a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function MaxI(const a, b: Integer): Integer;
begin
  if a < b then
    Result := b
  else
    Result := a;
end;

function CopyFile(const sname, dname: string): boolean;
var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
begin
  if FileExists(sname) then
  begin
    AssignFile(FromF, sname);
    Reset(FromF, 1);
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
    Result := True;
  end
  else
    Result := False;
end;

procedure BackupFile(const fname: string);
var
  fbck: string;
begin
  if not FileExists(fname) then
    Exit;
  fbck := fname + '_bak';
  CopyFile(fname, fbck);
end;

function MkShortName(const fname: string): string;
const
  MAXDISPFNAME = 30;
var
  i: integer;
begin
  if Length(fname) < MAXDISPFNAME then
  begin
    Result := fname;
    exit;
  end;
  Result := '';
  for i := Length(fname) downto Length(fname) - (MAXDISPFNAME - 6) do
    Result := fname[i] + Result;
  Result := '...' + Result;
  for i := 3 downto 1 do
    Result := fname[i] + Result;
end;

type
  shellexecute_t = function (hWnd: HWND; Operation, FileName, Parameters,
    Directory: PChar; ShowCmd: Integer): HINST; stdcall;

procedure I_GoToWebPage(const cmd: string);
var
  shellexecutefunc: shellexecute_t;
  inst: THandle;
begin
  inst := LoadLibrary('shell32');
  shellexecutefunc := GetProcAddress(inst, 'ShellExecuteA');
  shellexecutefunc(0, 'open', PChar(cmd), nil, nil, SW_SHOWNORMAL);
  FreeLibrary(inst);
end;

end.

