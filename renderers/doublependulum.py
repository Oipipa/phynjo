import numpy as np, pandas as pd, pyglet, moderngl
from pyglet import gl

# load data
csv = 'data/doublePendulum.csv'
df = pd.read_csv(csv)
θ1, θ2 = df['θ1'].values, df['θ2'].values
frames = len(θ1)

# geometry in world units
L1 = L2 = 1.0
x1, y1 = L1 * np.sin(θ1), -L1 * np.cos(θ1)
x2, y2 = x1 + L2 * np.sin(θ2), y1 - L2 * np.cos(θ2)

# normalise to NDC
extents = max(np.abs(np.concatenate((x1, x2, y1, y2)))) * 1.15
P1 = np.stack((x1 / extents, y1 / extents), 1).astype('f4')
P2 = np.stack((x2 / extents, y2 / extents), 1).astype('f4')
origin = np.zeros(2, dtype='f4')

# pyglet / moderngl setup
config = gl.Config(sample_buffers=1, samples=4, depth_size=16, double_buffer=True)
window = pyglet.window.Window(1024, 1024, 'double pendulum', resizable=False, config=config)
ctx = moderngl.create_context()
ctx.enable(moderngl.BLEND)
ctx.blend_func = moderngl.SRC_ALPHA, moderngl.ONE_MINUS_SRC_ALPHA

prog = ctx.program(
    vertex_shader='''#version 330\nin vec2 in_pos;\nvoid main(){gl_Position = vec4(in_pos, 0.0, 1.0);}''',
    fragment_shader='''#version 330\nuniform vec4 color;\nout vec4 out_color;\nvoid main(){out_color = color;}'''
)

# buffers
buf_rod = ctx.buffer(reserve=24)           # 3 × vec2
buf_p1 = ctx.buffer(reserve=8)
buf_p2 = ctx.buffer(reserve=8)
buf_trail = ctx.buffer(reserve=frames * 8) # full path

vao_rod = ctx.simple_vertex_array(prog, buf_rod, 'in_pos')
vao_p1 = ctx.simple_vertex_array(prog, buf_p1, 'in_pos')
vao_p2 = ctx.simple_vertex_array(prog, buf_p2, 'in_pos')
vao_trail = ctx.simple_vertex_array(prog, buf_trail, 'in_pos')

# state
step = 1 / 60
frame = 0
trail_vertices = 0

@window.event
def on_draw():
    window.clear()
    ctx.clear(0.03, 0.03, 0.05)

    # rods
    ctx.line_width = 4
    prog['color'].value = (1, 1, 1, 1)
    vao_rod.render(moderngl.LINE_STRIP)

    # persistent trail
    ctx.line_width = 2
    prog['color'].value = (0.2, 0.8, 1.0, 0.6)
    if trail_vertices:
        vao_trail.render(moderngl.LINE_STRIP, vertices=trail_vertices)

    # masses
    ctx.point_size = 18
    prog['color'].value = (0.9, 0.2, 0.3, 1)
    vao_p1.render(moderngl.POINTS)
    prog['color'].value = (0.2, 0.5, 0.9, 1)
    vao_p2.render(moderngl.POINTS)

def update(_):
    global frame, trail_vertices

    p1 = P1[frame]
    p2 = P2[frame]

    # update rod and masses
    rod_data = np.concatenate((origin, p1, p2)).astype('f4')
    buf_rod.write(rod_data.tobytes())
    buf_p1.write(p1.tobytes())
    buf_p2.write(p2.tobytes())

    # append to trail once per frame (persistent)
    if trail_vertices < frames:
        buf_trail.write(p2.tobytes(), offset=trail_vertices * 8)
        trail_vertices += 1

    frame = (frame + 1) % frames

# prime first frame so trail starts at correct location
update(0)

pyglet.clock.schedule_interval(update, step)
pyglet.app.run()
