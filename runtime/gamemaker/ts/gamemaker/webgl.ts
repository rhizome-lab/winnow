/** WebGL2 utilities for GML shader support. */

export interface ShaderProgram {
  program: WebGLProgram;
  uniformLocations: Map<string, WebGLUniformLocation>;
}

/**
 * Compile and link a GLSL ES vertex + fragment shader pair.
 * Returns null on compilation or link failure (errors logged to console).
 */
export function compileProgram(
  gl: WebGL2RenderingContext,
  vert: string,
  frag: string,
): ShaderProgram | null {
  const vs = compileShader(gl, gl.VERTEX_SHADER, vert);
  if (!vs) return null;
  const fs = compileShader(gl, gl.FRAGMENT_SHADER, frag);
  if (!fs) { gl.deleteShader(vs); return null; }

  const program = gl.createProgram();
  if (!program) return null;
  gl.attachShader(program, vs);
  gl.attachShader(program, fs);
  gl.linkProgram(program);
  gl.deleteShader(vs);
  gl.deleteShader(fs);

  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    console.error("Shader link error:", gl.getProgramInfoLog(program));
    gl.deleteProgram(program);
    return null;
  }

  return { program, uniformLocations: new Map() };
}

function compileShader(gl: WebGL2RenderingContext, type: number, src: string): WebGLShader | null {
  const shader = gl.createShader(type);
  if (!shader) return null;
  gl.shaderSource(shader, src);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    console.error(
      type === gl.VERTEX_SHADER ? "Vertex" : "Fragment",
      "shader compile error:",
      gl.getShaderInfoLog(shader),
    );
    gl.deleteShader(shader);
    return null;
  }
  return shader;
}

/**
 * Build a column-major 4×4 orthographic projection matrix mapping
 * [0,w] × [0,h] → NDC, with y flipped (top-left origin) and z depth [-1,1].
 */
export function orthoMatrix(w: number, h: number): Float32Array {
  // Column-major layout for gl.uniformMatrix4fv
  return new Float32Array([
    2 / w,  0,      0,  0,
    0,     -2 / h,  0,  0,
    0,      0,     -1,  0,
   -1,      1,      0,  1,
  ]);
}

/**
 * Create a VAO with a fullscreen clip-space quad (-1..1) plus UV coords (0..1).
 * Attribute 0 = vec2 position, attribute 1 = vec2 texcoord.
 */
export function makeFullscreenQuad(gl: WebGL2RenderingContext): WebGLVertexArrayObject | null {
  const vao = gl.createVertexArray();
  if (!vao) return null;
  gl.bindVertexArray(vao);

  const buf = gl.createBuffer();
  if (!buf) { gl.deleteVertexArray(vao); return null; }

  // Two triangles covering the full screen: position (xy) + texcoord (uv)
  const data = new Float32Array([
    // x     y     u    v
    -1.0,  1.0,  0.0, 1.0,
    -1.0, -1.0,  0.0, 0.0,
     1.0,  1.0,  1.0, 1.0,
     1.0, -1.0,  1.0, 0.0,
  ]);
  gl.bindBuffer(gl.ARRAY_BUFFER, buf);
  gl.bufferData(gl.ARRAY_BUFFER, data, gl.STATIC_DRAW);

  const stride = 4 * 4; // 4 floats × 4 bytes
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 2, gl.FLOAT, false, stride, 0);
  gl.enableVertexAttribArray(1);
  gl.vertexAttribPointer(1, 2, gl.FLOAT, false, stride, 2 * 4);

  gl.bindVertexArray(null);
  return vao;
}
