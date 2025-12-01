if ($args.Count -ge 1) {
    $filename = $args[0]
}

else {
    Write-Error  "Please provide file name (e.g. 'day1')"
    exit 1
}

$outDir  = 'output'
$exePath    = Join-Path $outDir "$filename.exe"

Write-Host "Compiling $filename..."
& gfortran -std=legacy -o $exePath "$filename.f"


if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile $filename.f"
    exit $LASTEXITCODE
}

Write-Host "Running $exePath..."
& $exePath
