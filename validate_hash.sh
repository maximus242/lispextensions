#!/bin/bash

# Compute expected hash for the known outputs
EXPECTED_HASH_MAIN=$(openssl dgst -sha256 expected_main.bin | awk '{print $2}')
EXPECTED_HASH_ADD_PS=$(openssl dgst -sha256 expected_vaddps_mm256_add_ps.bin | awk '{print $2}')
EXPECTED_HASH_FMA=$(openssl dgst -sha256 expected_fma.bin | awk '{print $2}')
EXPECTED_HASH_LOAD_PS=$(openssl dgst -sha256 expected_vmovaps_mm256_load_ps.bin | awk '{print $2}')
EXPECTED_HASH_BROADCAST=$(openssl dgst -sha256 expected_vbroadcastf128_mm256_broadcast_ps.bin | awk '{print $2}')

# Print expected hashes
echo "Recomputed expected hash for main.scm: $EXPECTED_HASH_MAIN"
echo "Recomputed expected hash for vfmadd132ps_mm256_fmadd_ps.scm: $EXPECTED_HASH_ADD_PS"
echo "Recomputed expected hash for fma.scm: $EXPECTED_HASH_FMA"
echo "Recomputed expected hash for load_ps.scm: $EXPECTED_HASH_LOAD_PS"
echo "Recomputed expected hash for vbroadcastf128_mm256_broadcast_ps.scm: $EXPECTED_HASH_BROADCAST"

# Compute SHA256 hash of binary_output.bin
if [ -f "binary_output.bin" ]; then
    COMPUTED_HASH=$(openssl dgst -sha256 binary_output.bin | awk '{print $2}')
    echo "Computed hash: $COMPUTED_HASH"
else
    echo "Error: binary_output.bin does not exist."
    exit 1
fi

# Compare the computed hash with the expected hashes
if [ "$COMPUTED_HASH" == "$EXPECTED_HASH_MAIN" ]; then
    echo "Validation successful for main.scm: Hash matches the expected value."
elif [ "$COMPUTED_HASH" == "$EXPECTED_HASH_ADD_PS" ]; then
    echo "Validation successful for vaddps_mm256_add_ps.scm: Hash matches the expected value."
elif [ "$COMPUTED_HASH" == "$EXPECTED_HASH_FMA" ]; then
    echo "Validation successful for vfmadd132ps_mm256_fmadd_ps.scm: Hash matches the expected value."
elif [ "$COMPUTED_HASH" == "$EXPECTED_HASH_LOAD_PS" ]; then
    echo "Validation successful for vmovaps_mm256_load_ps.scm: Hash matches the expected value."
elif [ "$COMPUTED_HASH" == "$EXPECTED_HASH_BROADCAST" ]; then
    echo "Validation successful for vbroadcastf128_mm256_broadcast_ps.scm: Hash matches the expected value."
else
    echo "Validation failed: Computed hash $COMPUTED_HASH does not match the expected value."
fi
