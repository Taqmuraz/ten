layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texcoord;
layout (location = 2) in vec3 normal;
layout (location = 3) in vec4 joints;
layout (location = 4) in vec4 weights;

out vec2 uv;
out vec3 worldNormal;
uniform mat4 transform;
uniform mat4 projection;

const int MAX_TRANSFORMS = 100;
uniform mat4 jointTransforms[MAX_TRANSFORMS];
uniform mat4 jointOffsets[MAX_TRANSFORMS];

void main (void)
{
	vec3 totalPos = vec3(0);
	vec3 totalNormal = vec3(0);

	for (int i = 0; i < 4; i++)
	{
		int j = int(joints[i]);
		if (j != -1)
		{
		  mat4 t = jointTransforms[j] * jointOffsets[j];
			totalPos += (transform * t * vec4(position, 1)).xyz * weights[i];
			totalNormal += (transform * t * vec4(normal, 0)).xyz * weights[i];
		}
	}
	int i = gl_InstanceID;
	float x = int(i - floor(i / 10) * 10) - 9;
	float z = int(i / 10);
	float y = int(i / 20);
	totalPos += vec3(x, y, z);
	
	gl_Position = projection * vec4(totalPos, 1);
	uv = texcoord;
	worldNormal = normalize(totalNormal);
}
