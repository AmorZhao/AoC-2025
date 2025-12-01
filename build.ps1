if ($args.Count -ge 1) {
    $filename = $args[0]
}

else {
    Write-Error  "Please provide file name (e.g. 'day1')"
    exit 1
}

$src     = Join-Path $PSScriptRoot '2025'
$helpers = Join-Path $PSScriptRoot 'helpers'
$outDir  = Join-Path $src 'output'
$ioHelperObj      = Join-Path $outDir 'io_utils.o'
$stringHelperObj = Join-Path $outDir 'string_utils.o'
$fileObj    = Join-Path $outDir "$filename.o"
$exePath    = Join-Path $outDir "$filename.exe"

Write-Host "Building io_utils module..."
& gfortran -c -g (Join-Path $helpers 'io_utils.f90') -J $outDir -o $ioHelperObj

if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile io_utils.f90"
    exit $LASTEXITCODE
}

Write-Host "Building string_utils module..."
& gfortran -c -g (Join-Path $helpers 'string_utils.f90') -J $outDir -o $stringHelperObj

if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile string_utils.f90"
    exit $LASTEXITCODE
}

Write-Host "Building $filename..."
& gfortran -c -g (Join-Path $src "$filename.f90") -I $outDir -o $fileObj

if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile $filename.f90"
    exit $LASTEXITCODE
}

Write-Host "Linking..."
& gfortran -g $ioHelperObj $stringHelperObj $fileObj -o $exePath

if ($LASTEXITCODE -ne 0) {
    Write-Error "Link failed"
    exit $LASTEXITCODE
}

Write-Host "Running $filename..."
& $exePath
