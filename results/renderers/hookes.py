import numpy as np, pandas as pd, pyglet, moderngl

df = pd.read_csv('../data/2bodyhookes.csv')
x1 = df['x1'].values
x2 = df['x2'].values
n = len(x1)
c = (x1.min() + x1.max() + x2.min() + x2.max()) / 4
s = max(abs(x1 - c).max(), abs(x2 - c).max()) * 1.2
p1 = np.stack(((x1 - c) / s * 0.9, np.zeros(n)), 1).astype('f4')
p2 = np.stack(((x2 - c) / s * 0.9, np.zeros(n)), 1).astype('f4')

w = pyglet.window.Window(800, 600, 'two-body', resizable=False)
ctx = moderngl.create_context()
prog = ctx.program(vertex_shader='''#version 330\nin vec2 in_pos;\nvoid main(){gl_Position=vec4(in_pos,0,1);}''', fragment_shader='''#version 330\nuniform vec3 color;\nout vec4 f;\nvoid main(){f=vec4(color,1);}''')

b1 = ctx.buffer(reserve=8)
b2 = ctx.buffer(reserve=8)
bl = ctx.buffer(reserve=16)

v1 = ctx.simple_vertex_array(prog, b1, 'in_pos')
v2 = ctx.simple_vertex_array(prog, b2, 'in_pos')
vl = ctx.simple_vertex_array(prog, bl, 'in_pos')

i = 0
dt = 1 / 60

@w.event
def on_draw():
    ctx.clear(0.02, 0.02, 0.02)
    prog['color'].value = (1, 1, 1)
    vl.render(moderngl.LINES)
    ctx.point_size = 20
    prog['color'].value = (1, 0, 0)
    v1.render(moderngl.POINTS)
    prog['color'].value = (0, 0, 1)
    v2.render(moderngl.POINTS)

def u(_):
    global i
    b1.write(p1[i].tobytes())
    b2.write(p2[i].tobytes())
    bl.write(np.concatenate((p1[i], p2[i])).tobytes())
    i = (i + 1) % n

pyglet.clock.schedule_interval(u, dt)
pyglet.app.run()
