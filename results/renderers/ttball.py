import numpy as np
import glfw
import moderngl
import pyrr
import time

def create_sphere(r, sectors, stacks):
    verts, norms, inds = [], [], []
    for i in range(stacks + 1):
        phi = np.pi/2 - i * np.pi / stacks
        y = r * np.sin(phi); hr = r * np.cos(phi)
        for j in range(sectors + 1):
            theta = j * 2 * np.pi / sectors
            x = hr * np.cos(theta); z = hr * np.sin(theta)
            verts += [x, y, z]; norms += [x/r, y/r, z/r]
    for i in range(stacks):
        for j in range(sectors):
            p1 = i*(sectors+1) + j; p2 = p1 + sectors+1
            inds += [p1,p2,p1+1,p2,p2+1,p1+1]
    return np.array(verts,'f4'), np.array(norms,'f4'), np.array(inds,'i4')

def create_grid(x0,x1,y0,y1,steps):
    g=[]
    for k in range(steps+1):
        t=k/steps
        x=x0*(1-t)+x1*t; g+=[x,y0,0,x,y1,0]
        y=y0*(1-t)+y1*t; g+=[x0,y,0,x1,y,0]
    return np.array(g,'f4')

glfw.init()
win=glfw.create_window(1024,768,"",None,None)
glfw.make_context_current(win)
ctx=moderngl.create_context()
ctx.enable(moderngl.DEPTH_TEST)

data=np.genfromtxt('../data/ttball.csv',delimiter=',',names=True)
pos=np.vstack((data['x'],data['y'],data['z'])).T.astype('f4')
qw,qx,qy,qz=data['qw'],data['qx'],data['qy'],data['qz']
wx,wy,wz=data['wx'],data['wy'],data['wz']
w_vecs=np.vstack((wx,wy,wz)).T
w_norms=np.linalg.norm(w_vecs,axis=1)
max_w=np.max(w_norms)
xmin,ymin=pos[:,:2].min(axis=0)
xmax,ymax=pos[:,:2].max(axis=0)

buf_grid=ctx.buffer(create_grid(xmin,xmax,ymin,ymax,2).tobytes())
buf_traj=ctx.buffer(pos.tobytes())
v_s,n_s,i_s=create_sphere(0.02,64,32)
buf_vs=ctx.buffer(v_s.tobytes()); buf_ns=ctx.buffer(n_s.tobytes()); buf_is=ctx.buffer(i_s.tobytes())
buf_arrow=ctx.buffer(reserve=6*4)

vs_flat='''
#version 330
in vec3 in_position;
uniform mat4 view,proj;
void main(){
    gl_Position=proj*view*vec4(in_position,1.0);
}
'''
fs_flat='''
#version 330
uniform vec4 color;
out vec4 f_color;
void main(){
    f_color=color;
}
'''
prog_flat=ctx.program(vertex_shader=vs_flat,fragment_shader=fs_flat)
vao_grid=ctx.simple_vertex_array(prog_flat,buf_grid,'in_position')
vao_traj=ctx.simple_vertex_array(prog_flat,buf_traj,'in_position')
vao_arrow=ctx.simple_vertex_array(prog_flat,buf_arrow,'in_position')

vs_sph='''
#version 330
in vec3 in_position; in vec3 in_normal;
uniform mat4 model,view,proj;
out vec3 v_norm_world; out vec3 v_norm_local; out vec3 v_pos;
void main(){
    v_norm_world=mat3(transpose(inverse(model)))*in_normal;
    v_norm_local=in_normal;
    v_pos=vec3(model*vec4(in_position,1.0));
    gl_Position=proj*view*model*vec4(in_position,1.0);
}
'''
fs_sph='''
#version 330
in vec3 v_norm_world; in vec3 v_norm_local; in vec3 v_pos;
uniform vec3 light;
out vec4 f_color;
vec3 gradientColor(float t){
    return mix(vec3(0.0,0.0,1.0),vec3(1.0,0.0,0.0),clamp(t,0.0,1.0));
}
void main(){
    vec3 n=normalize(v_norm_world);
    vec3 L=normalize(light-v_pos);
    float diff=max(dot(n,L),0.0);
    float t=v_norm_local.x*0.5+0.5;
    vec3 base=gradientColor(t);
    vec3 col=base*diff+0.1*base;
    f_color=vec4(col,1.0);
}
'''
prog_sph=ctx.program(vertex_shader=vs_sph,fragment_shader=fs_sph)
vao_sph=ctx.vertex_array(prog_sph,[(buf_vs,'3f','in_position'),(buf_ns,'3f','in_normal')],buf_is)

view=None
proj=pyrr.Matrix44.perspective_projection(45.0,1024/768,0.1,10.0,dtype='f4')
prog_flat['proj'].write(proj)
prog_sph['proj'].write(proj)
prog_sph['light'].value=(2.0,2.0,2.0)

cam_pos=pyrr.Vector3([1,1,1],dtype='f4')
offset=pyrr.Vector3([0.75,0.75,0.4],dtype='f4')
off_mag=np.linalg.norm(offset)
alpha=0.000025
idx=speed=4
while not glfw.window_should_close(win):
    p=pyrr.Vector3(pos[idx])
    desired=p+offset
    cam_pos=cam_pos+alpha*(desired-cam_pos)
    d=cam_pos-p; d/=np.linalg.norm(d); cam_pos=p+d*off_mag
    view=pyrr.Matrix44.look_at(cam_pos,p,pyrr.Vector3([0.0,0.0,1.0]),dtype='f4')
    prog_flat['view'].write(view); prog_sph['view'].write(view)

    ctx.clear(0.2,0.2,0.2)
    prog_flat['color'].value=(0.8,0.8,0.8,0.3); vao_grid.render(mode=moderngl.LINES)
    prog_flat['color'].value=(0.2,0.4,0.8,1.0); vao_traj.render(mode=moderngl.LINE_STRIP)

    trans=pyrr.matrix44.create_from_translation(p,dtype='f4')
    quat=pyrr.quaternion.create(x=qx[idx],y=qy[idx],z=qz[idx],w=qw[idx],dtype='f4')
    rot=pyrr.matrix44.create_from_quaternion(quat,dtype='f4')
    model=rot@trans
    prog_sph['model'].write(model); vao_sph.render()

    wmag=w_norms[idx]
    if wmag>1e-8:
        wdir=w_vecs[idx]/wmag
        al=0.1*(wmag/max_w)
        arr=np.array([p.x,p.y,p.z,p.x+wdir[0]*al,p.y+wdir[1]*al,p.z+wdir[2]*al],'f4')
        buf_arrow.write(arr.tobytes())
        prog_flat['color'].value=(1.0,1.0,0.0,1.0); vao_arrow.render(mode=moderngl.LINES)

    glfw.swap_buffers(win); glfw.poll_events()
    idx=(idx+speed)%pos.shape[0]; time.sleep(1/60)

glfw.terminate()
