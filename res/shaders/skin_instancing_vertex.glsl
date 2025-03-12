layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texcoord;
layout (location = 2) in vec3 normal;
layout (location = 3) in vec4 joints;
layout (location = 4) in vec4 weights;

out vec2 uv;
out vec3 worldNormal;
flat out int id;
uniform mat4 projection;
uniform float bones;

layout(binding = 5, std430) readonly buffer ssbo1 {
    mat4 transforms[];
};
layout(binding = 6, std430) readonly buffer ssbo2 {
    mat4 jointOffsets[];
};
layout(binding = 7, std430) readonly buffer ssbo3 {
    mat4 jointTransforms[];
};

void main (void)
{
	vec3 totalPos = vec3(0);
	vec3 totalNormal = vec3(0);
	id = gl_InstanceID;
	mat4 transform = transforms[id];

	for (int i = 0; i < 4; i++)
	{
		int j = int(joints[i]);
		if (j != -1)
		{
		  mat4 t = jointTransforms[j + (id * int(bones))] * jointOffsets[j];
			totalPos += (transform * t * vec4(position, 1)).xyz * weights[i];
			totalNormal += (transform * t * vec4(normal, 0)).xyz * weights[i];
		}
	}
	
	gl_Position = projection * vec4(totalPos, 1);
	uv = texcoord;
	worldNormal = normalize(totalNormal);
}
