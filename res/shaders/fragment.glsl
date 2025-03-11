in vec2 uv;
in vec3 worldNormal;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec4 color;
uniform vec3 worldLight;

void main (void)
{
  float l = -dot(worldLight, worldNormal) * 0.5 + 0.5;
	vec3 c = texture(texture2d, uv).rgb * color.rgb * l;
	out_Color = vec4(c, 1);
}
