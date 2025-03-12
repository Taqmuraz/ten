in vec3 position;
in vec2 texcoord;
in vec3 normal;

out vec2 uv;
out vec3 worldNormal;
flat out int id;
uniform mat4 transform;
uniform mat4 projection;

void main (void)
{
	uv = texcoord;
	id = gl_InstanceID;
	worldNormal = normalize((transform * vec4(normal, 0)).xyz);
	gl_Position = (projection * transform) * vec4(position, 1);
}
