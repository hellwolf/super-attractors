pragma circom 2.0.0;

// ref: https://blog.trailofbits.com/2023/03/21/circomspect-static-analyzer-circom-more-passes/
function signum(x) {
    if (x == 0) return 0;
    else if (x < 0) return -1;
    else return 1;
}

function abs(x) {
    if (x < 0) return -x;
    else return x;
}

function fixed18d_mul(a, b) {
    return signum(a) * signum(b) *
        (abs(a) * abs(b) \ 1000000000000000000);
}

template lorenz_attractor(N) {
    signal input sigma;
    signal input rho;
    signal input beta;
    signal input dt;
    signal input x0;
    signal input y0;
    signal input z0;

    signal output cx[N];
    signal output cy[N];
    signal output cz[N];
    signal output dx[N];
    signal output dy[N];
    signal output dz[N];
    signal output xy[N];
    signal output x[N];
    signal output y[N];
    signal output z[N];

    log("sigma", sigma, "rho", rho, "beta", beta, "dt", dt);

    x[0] <== x0;
    y[0] <== y0;
    z[0] <== z0;

    for(var i = 0; i < N - 1; i++){
        // dx = dt * (sigma * (y - x))
        cx[i+1] <-- fixed18d_mul(sigma, y[i] - x[i]);
        dx[i+1] <-- fixed18d_mul(dt, cx[i+1]);
        // dy = dt * (x * (rho - z) - y)
        cy[i+1] <-- fixed18d_mul(x[i], rho - z[i]) - y[i];
        dy[i+1] <-- fixed18d_mul(dt, cy[i+1]);
        // dz = dt * (x * y - beta * z)
        xy[i+1] <-- fixed18d_mul(x[i], y[i]);
        cz[i+1] <-- xy[i+1] - fixed18d_mul(beta, z[i]);
        dz[i+1] <-- fixed18d_mul(dt, cz[i+1]);

        log("x_i", x[i], "y_i", y[i], "z_i", z[i]);
        log("xy_n", xy[i+1]);
        log("cx_n", cx[i+1], "cy_n", cy[i+1], "cz_n", cz[i+1]);
        log("dx_n", dx[i+1], "dy_n", dy[i+1], "dz_n", dz[i+1]);

        var nx = x[i] + dx[i+1];
        var ny = y[i] + dy[i+1];
        var nz = z[i] + dz[i+1];
        log("x_n", nx, "y_n", ny, "z_n", nz);

        x[i+1] <== nx;
        y[i+1] <== ny;
        z[i+1] <== nz;
    }
}

template lorenz_attractor_verifier(N) {
    signal input sigma;
    signal input rho;
    signal input beta;
    signal input dt;
    signal input x[N];
    signal input y[N];
    signal input z[N];

    component c = lorenz_attractor(N);
    c.sigma <== sigma;
    c.rho   <== rho;
    c.beta  <== beta;
    c.dt    <== dt;
    c.x0    <== x[0];
    c.y0    <== y[0];
    c.z0    <== z[0];
    for(var i = 0; i < N - 1; i++){
        c.x[i] === x[i];
        c.y[i] === y[i];
        c.z[i] === z[i];
    }
}

component main = lorenz_attractor_verifier(1000);

// 10000000000000000000,
// 28000000000000000000,
// 2666666666666666496,
// 1000000000000000
