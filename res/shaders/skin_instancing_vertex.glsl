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
uniform float frames;

layout(binding = 5, std430) readonly buffer ssbo1 {
    mat4 transforms[];
};
layout(binding = 6, std430) readonly buffer ssbo2 {
    mat4 jointOffsets[];
};
layout(binding = 7, std430) readonly buffer ssbo3 {
    float shift[];
};
layout(binding = 9, std430) readonly buffer ssbo5 {
    float times[];
};
layout(binding = 10, std430) readonly buffer ssbo6 {
    float animInds[];
};
layout(binding = 11, std430) readonly buffer ssbo7 {
    float animLens[];
};
layout(binding = 12, std430) readonly buffer ssbo8 {
    mat4 anims[];
};

mat4 lerp(mat4 a, mat4 b, float p) {
  return a + (b - a) * p;
}

void main (void)
{
	vec3 totalPos = vec3(0);
	vec3 totalNormal = vec3(0);
	id = gl_InstanceID;
	mat4 transform = transforms[id];
	int anim = int(animInds[id]);
	float animTime = times[id];
	float animLen = animLens[id];
	float animFrame = (animTime / animLen * frames);
	float animF = animFrame - (frames * floor(animFrame / frames));
	if (animF >= frames) animF -= frames;
	int low = int(floor(animF));
	int high = int(ceil(animF));
	if (high >= frames) high = 0;
	float param = animF - low;

	for (int i = 0; i < 4; i++)
	{
		int j = int(joints[i]);
		if (j != -1)
		{
		  int b = int(shift[j]);
		  int l = b + int(anim * frames + low) * int(bones);
		  int h = b + int(anim * frames + high) * int(bones);
		  mat4 tl = anims[l];
		  mat4 th = anims[h];
		  mat4 t = lerp(tl, th, param) * jointOffsets[j];
			totalPos += (t * vec4(position, 1)).xyz * weights[i];
			totalNormal += (t * vec4(normal, 0)).xyz * weights[i];
		}
	}
	
	gl_Position = projection * transform * vec4(totalPos, 1);
	uv = texcoord;
	worldNormal = normalize((transform * vec4(totalNormal, 0)).xyz);
}
