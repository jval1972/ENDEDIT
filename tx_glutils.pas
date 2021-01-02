//------------------------------------------------------------------------------
//
//  DD_TEXTURE: A tool for creating textures from real world photos.
//  Copyright (C) 2017-2021 by Jim Valavanis
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
//  OpenGL utilities
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/texture-perspective/
//------------------------------------------------------------------------------

unit tx_glutils;

interface

uses
  dglOpenGL;

type
  vertexinfo2d_t = record
    xy: array[0..1] of GLfloat;
    uv: array[0..1] of GLfloat;
  end;
  vertexinfo2d_p = ^vertexinfo2d_t;

procedure DOVERTEX2D(const vi: vertexinfo2d_t);

implementation

procedure DOVERTEX2D(const vi: vertexinfo2d_t);
begin
  glTexCoord2fv(@vi.uv);
  glVertex2fv(@vi.xy);
end;


end.
