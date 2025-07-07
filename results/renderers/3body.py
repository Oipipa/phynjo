import pandas as pd
import numpy as np
import pyglet
import moderngl
from pyrr import Matrix44

data = pd.read_csv('../data/3bodyproblem.csv')
positions = np.stack([data[['x1','y1','z1']].values,
                      data[['x2','y2','z2']].values,
                      data[['x3','y3','z3']].values], axis=1).astype('f4')
pos_all = positions.reshape(-1, 3)
center = pos_all.mean(axis=0)
positions -= center
radius = np.linalg.norm(pos_all - center, axis=1).max()
positions *= (5.0 / radius)

colors = np.array([[1, 0, 0], [0, 1, 0], [0, 0, 1]], dtype='f4')

window = pyglet.window.Window(800, 600, resizable=True)
ctx = moderngl.create_context()
prog = ctx.program(
    vertex_shader='''
        #version 330
        in vec3 in_pos;
        in vec3 in_col;
        uniform mat4 mvp;
        out vec3 v_col;
        void main(){
            v_col = in_col;
            gl_Position = mvp * vec4(in_pos, 1.0);
            gl_PointSize = 10.0;
        }
    ''',
    fragment_shader='''
        #version 330
        in vec3 v_col;
        out vec4 f_color;
        void main(){
            f_color = vec4(v_col, 1.0);
        }
    '''
)
vbo_pos = ctx.buffer(reserve=positions.shape[1] * positions.shape[2] * 4)
vbo_col = ctx.buffer(colors.tobytes())
vao = ctx.vertex_array(prog, [
    (vbo_pos, '3f', 'in_pos'),
    (vbo_col, '3f', 'in_col'),
])

frame = 0

@window.event
def on_resize(width, height):
    ctx.viewport = (0, 0, width, height)

@window.event
def on_draw():
    global frame
    ctx.clear(0, 0, 0, 1)
    ctx.enable(moderngl.DEPTH_TEST)
    proj = Matrix44.perspective_projection(45.0, window.width / window.height, 0.1, 100.0)
    angle = frame * 0.005
    view = Matrix44.from_translation([0, 0, -10]) @ Matrix44.from_y_rotation(angle)
    prog['mvp'].write((proj @ view).astype('f4').tobytes())
    vbo_pos.write(positions[frame].tobytes())
    vao.render(mode=moderngl.POINTS)

def update(dt):
    global frame
    frame = (frame + 1) % positions.shape[0]
    window.invalid = True

pyglet.clock.schedule_interval(update, 1/60.0)
pyglet.app.run()
