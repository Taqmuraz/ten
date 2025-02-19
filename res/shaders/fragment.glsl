in vec2 uv;
in vec3 worldNormal;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec4 color;

void main (void)
{	
	out_Color = texture(texture2d, vec2(uv.x, 1 - uv.y)) * color;
}
