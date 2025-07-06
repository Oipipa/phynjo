import re
import glfw
import moderngl
import numpy as np
from pyrr import Matrix44, Quaternion

with open('../data/rotatingbody.csv') as f:
    lines = [l for l in f if l.strip()]
quats = []
for l in lines:
    nums = re.findall(r'[-+]?\d*\.\d+|\d+', l)
    w, x, y, z = map(float, nums[1:5])
    quats.append(Quaternion([x, y, z, w]))

glfw.init()
w, h = 800, 600
win = glfw.create_window(w, h, "", None, None)
glfw.make_context_current(win)
ctx = moderngl.create_context()
ctx.enable(moderngl.DEPTH_TEST)

prog = ctx.program(
    vertex_shader="""
#version 330
in vec3 in_position;
in vec3 in_normal;
uniform mat4 model;
uniform mat4 mvp;
out vec3 v_normal;
void main() {
    v_normal = mat3(transpose(inverse(model))) * in_normal;
    gl_Position = mvp * vec4(in_position, 1.0);
}
""",
    fragment_shader="""
#version 330
in vec3 v_normal;
out vec4 f_color;
uniform vec3 light_dir;
uniform vec3 object_color;
void main() {
    float diff = max(dot(normalize(v_normal), normalize(light_dir)), 0.2);
    vec3 c = object_color * (0.2 + diff);
    f_color = vec4(c, 1.0);
}
"""
)

vertices = np.array([
    -1,-1, 1,  0, 0, 1,   1,-1, 1,  0, 0, 1,   1, 1, 1,  0, 0, 1,  -1, 1, 1,  0, 0, 1,
    -1,-1,-1,  0, 0,-1,  -1, 1,-1,  0, 0,-1,   1, 1,-1,  0, 0,-1,   1,-1,-1,  0, 0,-1,
    -1, 1,-1,  0, 1, 0,  -1, 1, 1,  0, 1, 0,   1, 1, 1,  0, 1, 0,   1, 1,-1,  0, 1, 0,
    -1,-1,-1,  0,-1, 0,   1,-1,-1,  0,-1, 0,   1,-1, 1,  0,-1, 0,  -1,-1, 1,  0,-1, 0,
    -1,-1,-1, -1, 0, 0,  -1,-1, 1, -1, 0, 0,  -1, 1, 1, -1, 0, 0,  -1, 1,-1, -1, 0, 0,
     1,-1, 1,  1, 0, 0,   1,-1,-1,  1, 0, 0,   1, 1,-1,  1, 0, 0,   1, 1, 1,  1, 0, 0
], dtype='f4')

indices = np.array([
     0, 1, 2,  2, 3, 0,
     4, 5, 6,  6, 7, 4,
     8, 9,10, 10,11, 8,
    12,13,14, 14,15,12,
    16,17,18, 18,19,16,
    20,21,22, 22,23,20
], dtype='i4')

vbo = ctx.buffer(vertices.tobytes())
ibo = ctx.buffer(indices.tobytes())
vao = ctx.vertex_array(prog, [(vbo, '3f 3f', 'in_position', 'in_normal')], ibo)

proj = Matrix44.perspective_projection(45.0, w/h, 0.1, 100.0)
view = Matrix44.look_at([3, 3, 3], [0, 0, 0], [0, 1, 0])

prog['light_dir'].value = (1.0, 1.0, 1.0)
prog['object_color'].value = (0.8, 0.9, 1.0)

i = 0
n = len(quats)
while not glfw.window_should_close(win):
    model = quats[i].matrix44
    mvp = proj * view * model
    prog['model'].write(model.astype('f4').tobytes())
    prog['mvp'].write(mvp.astype('f4').tobytes())
    ctx.clear(0.1, 0.1, 0.1)
    vao.render()
    glfw.swap_buffers(win)
    glfw.poll_events()
    i = (i + 1) % n

glfw.terminate()
