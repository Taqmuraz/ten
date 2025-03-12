in vec3 position;
in vec2 texcoord;
in vec3 normal;

out vec2 uv;
out vec3 worldNormal;
flat out int id;
uniform mat4 projection;

layout(binding = 5, std430) readonly buffer ssbo1 {
  mat4 transforms[];
};

void main (void)
{
	uv = texcoord;
	id = gl_InstanceID;
	mat4 transform = transforms[id];
	worldNormal = normalize((transform * vec4(normal, 0)).xyz);
	gl_Position = (projection * transform) * vec4(position, 1);
}
