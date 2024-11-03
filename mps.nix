{ lib
, stdenv
, fetchFromGitHub
, sqlite
}:

stdenv.mkDerivation rec {
  pname = "mps";
  version = "1.118.0";

  # Fetch the source code from GitHub
  src = fetchFromGitHub {
    owner = "Ravenbrook";
    repo = "mps";
    rev = "release-${version}";
    sha256 = "sha256-3ql3jWLccgnQHKf23B1en+nJ9rxqmHcWd7aBr93YER0="; # Verify this hash
  };

  # Apply patches after fetching the source
  postPatch = ''
            # Disable -Werror to prevent build failures due to warnings
            substituteInPlace code/gc.gmk --replace '-Werror ' ' '
            substituteInPlace code/gp.gmk --replace '-Werror ' ' '
            substituteInPlace code/ll.gmk --replace '-Werror ' ' '
          '';

  # Specify build dependencies
  buildInputs = [ sqlite ];

  # Define the build phase using the absolute path to xcodebuild and set DerivedData path
  buildPhase = ''
            echo "Building MPS on macOS using xcodebuild..."

            # Set the MACOSX_DEPLOYMENT_TARGET to match the xcodebuild target
            export MACOSX_DEPLOYMENT_TARGET=15.1

            cd code

            # Define a writable DerivedData path within the build environment
            DERIVED_DATA="$TMPDIR/DerivedData"

            # Create the DerivedData directory
            mkdir -p "$DERIVED_DATA"

            # Run xcodebuild with the specified DerivedData path
            /usr/bin/xcodebuild -scheme mps \
                                 -configuration Release \
                                 -project mps.xcodeproj \
                                 -derivedDataPath "$DERIVED_DATA" \
                                 OTHER_CFLAGS="-Wno-error=unused-but-set-variable -Wno-unused-but-set-variable -mmacos-version-min=15.1"
          '';

  # Define the install phase: copy the built library and headers to the output directory
  installPhase = ''
            echo "Installing MPS to $out..."

            mkdir -p $out/lib
            cp "$TMPDIR/source/code/xc/Release/libmps.a" $out/lib/

            mkdir -p $out/include
            cp mps*.h $out/include/
          '';

  # Metadata about the package
  meta = with lib; {
    description = "Flexible memory management and garbage collection library";
    homepage    = "https://www.ravenbrook.com/project/mps";
    license     = licenses.bsd2; # Matches Homebrew's BSD-2-Clause
    platforms   = platforms.darwin;
    maintainers = []; # You can add maintainers here if desired
  };
}

