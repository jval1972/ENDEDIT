//------------------------------------------------------------------------------
//
//  DD_TEXTURE: A tool for creating textures from real world photos.
//  Copyright (C) 2017 by Jim Valavanis
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
//  OpenGL Rendering
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/texture-perspective/
//------------------------------------------------------------------------------

unit tx_gl;

interface

uses
  Windows,
  Graphics,
  dglOpenGL;

var
  gld_max_texturesize: integer = 0;
  gl_tex_format: integer = GL_RGBA8;
  gl_tex_filter: integer = GL_LINEAR;
  PANELSIZE: integer;

procedure glInit(const psize: integer);

procedure glBeginScene(const Width, Height: integer);

procedure glEndScene(dc: HDC);

procedure gld_CreateTexture(const b: TBitmap);
procedure gld_ShutDownTexture;

var
  bitmaptexture: GLuint = $FFFFFFFF;

implementation

uses
  Classes;
  
{------------------------------------------------------------------}
{  Initialise OpenGL                                               }
{------------------------------------------------------------------}
procedure glInit(const psize: integer);
begin
  glClearColor(0.0, 0.0, 0.0, 0.0);   // Black Background
  glShadeModel(GL_SMOOTH);            // Enables Smooth Color Shading
  glClearDepth(1.0);                  // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);            // Enable Depth Buffer
  glDepthFunc(GL_LESS);                // The Type Of Depth Test To Do
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @gld_max_texturesize);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_POINT_SMOOTH);
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_LINE_SMOOTH);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_POLYGON_SMOOTH);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
  PANELSIZE := psize;
end;

procedure glBeginScene(const Width, Height: integer);
begin
  glDisable(GL_CULL_FACE);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  glMatrixMode(GL_MODELVIEW);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity;                                       // Reset The View
end;

procedure glEndScene(dc: HDC);
begin
  SwapBuffers(dc);                                // Display the scene
end;

type
  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..$FFFF] of LongWord;

const
  TEXTURESIZE = 2048;

procedure gld_CreateTexture(const b: TBitmap);
var
  buffer: PLongWordArray;
  line: PLongWordArray;
  dest: PLongWord;
  tmp: TBitmap;
  i, j: integer;
  texsize: integer;
begin
  gld_ShutDownTexture;

  if gld_max_texturesize < TEXTURESIZE then
    texsize := gld_max_texturesize
  else
    texsize := TEXTURESIZE;
  tmp := TBitmap.Create;
  tmp.Width := texsize;
  tmp.Height := texsize;
  tmp.PixelFormat := pf32bit;
  tmp.Canvas.StretchDraw(Rect(0, 0, texsize - 1, texsize - 1), b);

  GetMem(buffer, texsize * texsize * SizeOf(LongWord));
  dest := @buffer[0];

  for j := 0 to texsize - 1 do
  begin
    line := tmp.ScanLine[j];
    for i := 0 to texsize - 1 do
    begin
      dest^ := line[i];
      Inc(dest);
    end;
  end;
  tmp.Free;

  glGenTextures(1, @bitmaptexture);

  glBindTexture(GL_TEXTURE_2D, bitmaptexture);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8,
               texsize, texsize,
               0, GL_BGRA, GL_UNSIGNED_BYTE, buffer);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  FreeMem(buffer, texsize * texsize * SizeOf(LongWord));
end;


procedure gld_ShutDownTexture;
begin
  if bitmaptexture <> $FFFFFFFF then
    glDeleteTextures(1, @bitmaptexture);
end;

end.
