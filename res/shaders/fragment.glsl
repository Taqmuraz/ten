in vec2 uv;
in vec3 worldNormal;

out vec4 out_Color;

uniform sampler2D texture2d;

void main (void)
{	
	out_Color = vec4(worldNormal, 1);//texture(texture2d, uv) + vec4(worldNormal, 1);
}
