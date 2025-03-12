in vec2 uv;
in vec3 worldNormal;
flat in int id;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec3 worldLight;

layout(binding = 8, std430) readonly buffer ssbo4 {
    vec4 colors[];
};

void main (void)
{
  vec4 color = colors[id];
  float l = -dot(worldLight, worldNormal) * 0.5 + 0.5;
	vec3 c = texture(texture2d, uv).rgb * color.rgb * l;
	out_Color = vec4(c, 1);
}
