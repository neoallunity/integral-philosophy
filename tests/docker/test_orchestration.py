"""
Additional Docker container tests for Integral Philosophy Publishing System
"""

import pytest
import time
import json
import requests
import tempfile
import subprocess
import yaml
import os
import socket
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
from unittest.mock import Mock, patch, MagicMock

from ..utils.base_test_classes import BaseTestCase, IntegrationTestCase


class TestContainerBuild(BaseTestCase):
    """Test Docker container build processes"""

    def test_dockerfile_syntax_validation(self):
        """Validate Dockerfile syntax without building"""
        dockerfile_path = Path(__file__).parent.parent.parent / "Dockerfile"

        if not dockerfile_path.exists():
            pytest.skip("Dockerfile not found")

        with open(dockerfile_path, "r") as f:
            dockerfile_content = f.read()

        # Check for valid Dockerfile syntax
        lines = dockerfile_content.strip().split("\n")

        for i, line in enumerate(lines, 1):
            line = line.strip()
            if not line or line.startswith("#"):
                continue

            # Check for valid instructions
            valid_instructions = [
                "FROM",
                "MAINTAINER",
                "RUN",
                "CMD",
                "LABEL",
                "EXPOSE",
                "ENV",
                "ADD",
                "COPY",
                "ENTRYPOINT",
                "VOLUME",
                "USER",
                "WORKDIR",
                "ARG",
                "ONBUILD",
                "STOPSIGNAL",
                "HEALTHCHECK",
                "SHELL",
                "FROM",
            ]

            if not any(
                line.startswith(instruction) for instruction in valid_instructions
            ):
                pytest.fail(f"Invalid Dockerfile instruction at line {i}: {line}")

        # Validate FROM instruction is first
        non_comment_lines = [
            line for line in lines if line.strip() and not line.strip().startswith("#")
        ]
        if non_comment_lines and not non_comment_lines[0].startswith("FROM"):
            pytest.fail("Dockerfile must start with FROM instruction")

    def test_dockerfile_build_context_validation(self):
        """Validate Docker build context"""
        dockerfile_path = Path(__file__).parent.parent.parent / "Dockerfile"

        if not dockerfile_path.exists():
            pytest.skip("Dockerfile not found")

        with open(dockerfile_path, "r") as f:
            dockerfile_content = f.read()

        # Check for common build context issues
        context_issues = []

        lines = dockerfile_content.split("\n")
        for i, line in enumerate(lines):
            line = line.strip()

            # Check for COPY/ADD destination validation
            if line.startswith(("COPY", "ADD")):
                parts = line.split()
                if len(parts) >= 3:
                    source, destination = parts[1], parts[2]

                    # Check if destination is absolute or has proper path
                    if not destination.startswith("/") and not destination.startswith(
                        "./"
                    ):
                        context_issues.append(
                            f"Line {i + 1}: COPY/ADD destination should be absolute or relative path"
                        )

                    # Check for copying .dockerignore files
                    if ".dockerignore" in source:
                        context_issues.append(
                            f"Line {i + 1}: Should not copy .dockerignore file"
                        )

        if context_issues:
            pytest.fail("Build context issues found:\n" + "\n".join(context_issues))

    def test_multi_stage_build_validation(self):
        """Validate multi-stage build configuration"""
        dockerfile_path = Path(__file__).parent.parent.parent / "Dockerfile"

        if not dockerfile_path.exists():
            pytest.skip("Dockerfile not found")

        with open(dockerfile_path, "r") as f:
            dockerfile_content = f.read()

        # Check for multi-stage build patterns
        from_instructions = [
            line
            for line in dockerfile_content.split("\n")
            if line.strip().startswith("FROM")
        ]

        if len(from_instructions) > 1:
            # Multi-stage build detected
            print("Multi-stage build detected - validating configuration")

            # Check for proper stage naming
            stage_names = []
            for i, from_line in enumerate(from_instructions):
                parts = from_line.strip().split()
                if len(parts) > 2 and "as" in parts[2].lower():
                    stage_name = parts[3] if len(parts) > 3 else f"stage_{i}"
                    stage_names.append(stage_name)

            # Validate COPY --from usage
            copy_lines = [
                line
                for line in dockerfile_content.split("\n")
                if line.strip().startswith("COPY")
            ]

            for copy_line in copy_lines:
                if "--from=" in copy_line:
                    stage_ref = copy_line.split("--from=")[1].split()[0]
                    if stage_ref not in stage_names and not stage_ref.isdigit():
                        pytest.fail(f"Invalid stage reference in COPY: {stage_ref}")

    def test_layer_optimization(self):
        """Test Docker layer optimization best practices"""
        dockerfile_path = Path(__file__).parent.parent.parent / "Dockerfile"

        if not dockerfile_path.exists():
            pytest.skip("Dockerfile not found")

        with open(dockerfile_path, "r") as f:
            dockerfile_content = f.read()

        optimization_issues = []

        # Count layers
        layer_instructions = ["RUN", "COPY", "ADD", "WORKDIR", "USER", "ENV"]
        layer_count = sum(
            1
            for line in dockerfile_content.split("\n")
            if any(line.strip().startswith(inst) for inst in layer_instructions)
        )

        if layer_count > 15:
            optimization_issues.append(
                f"High layer count: {layer_count} layers (consider combining)"
            )

        # Check for RUN command chaining
        run_lines = [
            line
            for line in dockerfile_content.split("\n")
            if line.strip().startswith("RUN")
        ]

        if len(run_lines) > 5:
            optimization_issues.append(
                f"Many RUN instructions: {len(run_lines)} (consider chaining with &&)"
            )

        # Check for package manager cache cleanup
        for run_line in run_lines:
            run_content = run_line.strip()
            if any(cmd in run_content for cmd in ["apt-get", "yum", "apk"]):
                if "rm -rf" not in run_content and "/var/cache" not in run_content:
                    optimization_issues.append(
                        f"Package cache not cleaned in: {run_content[:50]}..."
                    )

        if optimization_issues:
            print("Layer optimization suggestions:\n" + "\n".join(optimization_issues))


class TestContainerNetworking(BaseTestCase):
    """Test container networking configurations"""

    def test_network_isolation(self):
        """Test network isolation configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        networks = compose_config.get("networks", {})
        services = compose_config.get("services", {})

        # Check for custom network usage
        custom_networks = [
            name
            for name, config in networks.items()
            if isinstance(config, dict) and config.get("driver") != "bridge"
        ]

        if len(custom_networks) == 0:
            print("No custom networks detected - using default bridge network")

        # Validate service network assignments
        for service_name, service_config in services.items():
            service_networks = service_config.get("networks", [])

            if isinstance(service_networks, list) and len(service_networks) > 1:
                # Service connects to multiple networks
                for network in service_networks:
                    if network not in networks:
                        pytest.fail(
                            f"Service {service_name} references undefined network: {network}"
                        )

    def test_port_exposure_security(self):
        """Test secure port exposure configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})
        security_issues = []

        for service_name, service_config in services.items():
            ports = service_config.get("ports", [])

            for port_mapping in ports:
                if isinstance(port_mapping, str):
                    # Parse port mapping "host:container"
                    if ":" in port_mapping:
                        host_port, container_port = port_mapping.split(":", 1)

                        # Check for binding to all interfaces
                        if "0.0.0.0:" in port_mapping or ":::" in port_mapping:
                            # Only allow this for web services
                            if service_name not in ["web", "nginx", "reverse-proxy"]:
                                security_issues.append(
                                    f"Service {service_name} binds to all interfaces: {port_mapping}"
                                )

                        # Check for privileged port usage
                        try:
                            host_port_num = int(host_port.split("-")[0])
                            if host_port_num < 1024:
                                print(
                                    f"Service {service_name} uses privileged port: {host_port}"
                                )
                        except ValueError:
                            pass  # Skip invalid port numbers

                    # Check for exposing database ports externally
                    if service_name in [
                        "db",
                        "database",
                        "postgres",
                        "mysql",
                    ] and isinstance(port_mapping, str):
                        if ":" in port_mapping:
                            security_issues.append(
                                f"Database service {service_name} exposes port externally: {port_mapping}"
                            )

        if security_issues:
            pytest.fail("Port exposure security issues:\n" + "\n".join(security_issues))

    def test_service_discovery(self):
        """Test service discovery configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})

        # Check for service dependencies
        for service_name, service_config in services.items():
            depends_on = service_config.get("depends_on", [])

            if depends_on:
                # Validate dependency exists
                for dependency in depends_on:
                    if dependency not in services:
                        pytest.fail(
                            f"Service {service_name} depends on non-existent service: {dependency}"
                        )

            # Check for service name usage in environment variables
            environment = service_config.get("environment", {})
            if isinstance(environment, dict):
                for env_key, env_value in environment.items():
                    # Check if environment variables reference other services
                    for other_service in services:
                        if (
                            other_service in env_value
                            and f"{other_service}:" not in env_value
                        ):
                            print(
                                f"Potential service reference in {service_name}: {env_key}={env_value}"
                            )

    def test_load_balancing_configuration(self):
        """Test load balancing configurations"""
        compose_files = [
            "docker-compose.yml",
            "docker-compose.prod.yml",
            "docker-compose.production.yml",
        ]

        base_path = Path(__file__).parent.parent.parent
        lb_config_found = False

        for compose_file in compose_files:
            compose_path = base_path / compose_file
            if not compose_path.exists():
                continue

            lb_config_found = True

            try:
                with open(compose_path, "r") as f:
                    compose_config = yaml.safe_load(f)
            except yaml.YAMLError as e:
                pytest.fail(f"Invalid {compose_file}: {e}")

            services = compose_config.get("services", {})

            # Check for load balancer services
            lb_services = ["nginx", "haproxy", "traefik", "apache"]
            found_lb = []

            for service_name in services:
                if any(lb in service_name.lower() for lb in lb_services):
                    found_lb.append(service_name)

                    service_config = services[service_name]
                    ports = service_config.get("ports", [])

                    # Load balancer should expose ports
                    if not ports:
                        print(f"Load balancer {service_name} doesn't expose any ports")

            if found_lb:
                print(f"Found load balancer services: {', '.join(found_lb)}")
            else:
                print(f"No load balancer services found in {compose_file}")

        if not lb_config_found:
            pytest.skip("No Docker Compose files found for load balancing validation")


class TestContainerLifecycle(BaseTestCase):
    """Test container lifecycle management"""

    def test_startup_order_validation(self):
        """Test service startup order dependencies"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})
        startup_order_issues = []

        # Define expected startup order
        expected_order = {
            "db": ["web", "api", "worker"],
            "redis": ["web", "api", "worker"],
            "web": [],
            "api": ["db"],
            "worker": ["db", "redis"],
        }

        for service_name, service_config in services.items():
            depends_on = service_config.get("depends_on", [])

            if service_name in expected_order:
                expected_deps = expected_order[service_name]
                for dep in depends_on:
                    if dep not in expected_deps:
                        startup_order_issues.append(
                            f"Unexpected dependency: {service_name} depends on {dep}"
                        )

        if startup_order_issues:
            pytest.fail("Startup order issues:\n" + "\n".join(startup_order_issues))

    def test_health_check_implementation(self):
        """Test health check implementations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})
        health_check_issues = []

        for service_name, service_config in services.items():
            health_check = service_config.get("healthcheck", {})

            if health_check:
                # Validate health check configuration
                test_cmd = health_check.get("test")
                if not test_cmd:
                    health_check_issues.append(
                        f"Service {service_name} health check missing test command"
                    )
                else:
                    # Check for common health check patterns
                    if isinstance(test_cmd, list):
                        cmd_str = " ".join(test_cmd)
                    else:
                        cmd_str = str(test_cmd)

                    # Check for web service health checks
                    if (
                        service_name == "web"
                        and "curl" not in cmd_str
                        and "wget" not in cmd_str
                    ):
                        health_check_issues.append(
                            f"Web service {service_name} should use HTTP health check"
                        )

                    # Check for database health checks
                    if (
                        service_name in ["db", "postgres", "mysql"]
                        and "pg_isready" not in cmd_str
                        and "mysqladmin" not in cmd_str
                    ):
                        health_check_issues.append(
                            f"Database service {service_name} should use database-specific health check"
                        )

                # Check timeout and interval
                timeout = health_check.get("timeout", "30s")
                interval = health_check.get("interval", "30s")
                retries = health_check.get("retries", 3)

                # Validate reasonable values
                if timeout == interval:
                    health_check_issues.append(
                        f"Service {service_name} health check timeout equals interval"
                    )

                if retries < 2:
                    health_check_issues.append(
                        f"Service {service_name} health check retries too low: {retries}"
                    )

        if health_check_issues:
            print("Health check recommendations:\n" + "\n".join(health_check_issues))

    def test_graceful_shutdown(self):
        """Test graceful shutdown configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})
        shutdown_issues = []

        for service_name, service_config in services.items():
            # Check for stop timeout configuration
            stop_grace_period = service_config.get("stop_grace_period")

            if not stop_grace_period:
                if service_name in ["web", "api", "worker"]:
                    shutdown_issues.append(
                        f"Service {service_name} missing stop_grace_period"
                    )
            else:
                # Validate stop timeout format
                if not stop_grace_period.endswith(
                    "s"
                ) and not stop_grace_period.endswith("m"):
                    shutdown_issues.append(
                        f"Service {service_name} has invalid stop_grace_period format: {stop_grace_period}"
                    )

            # Check for restart policies
            restart_policy = service_config.get("restart", "no")

            if restart_policy == "no" and service_name in ["web", "api"]:
                shutdown_issues.append(
                    f"Critical service {service_name} has no restart policy"
                )

        if shutdown_issues:
            print("Graceful shutdown recommendations:\n" + "\n".join(shutdown_issues))

    def test_resource_cleanup(self):
        """Test resource cleanup configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})
        cleanup_recommendations = []

        for service_name, service_config in services.items():
            # Check for volume cleanup
            volumes = service_config.get("volumes", [])

            # Anonymous volumes should have explicit cleanup
            anonymous_volumes = [
                v for v in volumes if not isinstance(v, str) or ":" not in v
            ]
            if anonymous_volumes:
                cleanup_recommendations.append(
                    f"Service {service_name} has anonymous volumes: {anonymous_volumes}"
                )

            # Check for tmpfs usage
            tmpfs = service_config.get("tmpfs", [])
            if isinstance(tmpfs, list) and tmpfs:
                print(
                    f"Service {service_name} uses tmpfs for temporary storage: {tmpfs}"
                )

        # Check for volume definitions
        volumes_section = compose_config.get("volumes", {})

        if not volumes_section:
            print(
                "No named volumes defined - consider using named volumes for data persistence"
            )

        if cleanup_recommendations:
            print(
                "Resource cleanup recommendations:\n"
                + "\n".join(cleanup_recommendations)
            )


class TestContainerPerformance(BaseTestCase):
    """Test container performance configurations"""

    def test_memory_limits(self):
        """Test memory limit configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})
        memory_issues = []

        for service_name, service_config in services.items():
            deploy_config = service_config.get("deploy", {}).get("resources", {})

            memory_limit = deploy_config.get("limits", {}).get("memory")
            memory_reservation = deploy_config.get("reservations", {}).get("memory")

            if not memory_limit and service_name in ["web", "api", "worker"]:
                memory_issues.append(f"Service {service_name} missing memory limit")

            if memory_limit and memory_reservation:
                # Validate memory reservation is reasonable compared to limit
                try:
                    limit_gb = float(memory_limit.replace("g", "").replace("m", ""))
                    reservation_gb = float(
                        memory_reservation.replace("g", "").replace("m", "")
                    )

                    if reservation_gb / limit_gb > 0.8:
                        memory_issues.append(
                            f"Service {service_name} memory reservation too close to limit: "
                            f"{memory_reservation} vs {memory_limit}"
                        )
                except ValueError:
                    pass  # Skip invalid format

        if memory_issues:
            print("Memory configuration recommendations:\n" + "\n".join(memory_issues))

    def test_cpu_limits(self):
        """Test CPU limit configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})
        cpu_issues = []

        for service_name, service_config in services.items():
            deploy_config = service_config.get("deploy", {}).get("resources", {})

            cpu_limit = deploy_config.get("limits", {}).get("cpus")
            cpu_reservation = deploy_config.get("reservations", {}).get("cpus")

            if not cpu_limit and service_name in ["web", "api", "worker"]:
                cpu_issues.append(f"Service {service_name} missing CPU limit")

            if cpu_limit and cpu_reservation:
                # Validate CPU reservation is reasonable
                try:
                    limit_float = float(str(cpu_limit))
                    reservation_float = float(str(cpu_reservation))

                    if reservation_float / limit_float > 0.9:
                        cpu_issues.append(
                            f"Service {service_name} CPU reservation too close to limit: "
                            f"{cpu_reservation} vs {cpu_limit}"
                        )
                except ValueError:
                    pass  # Skip invalid format

        if cpu_issues:
            print("CPU configuration recommendations:\n" + "\n".join(cpu_issues))

    def test_storage_performance(self):
        """Test storage performance configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        services = compose_config.get("services", {})
        storage_recommendations = []

        for service_name, service_config in services.items():
            volumes = service_config.get("volumes", [])
            tmpfs = service_config.get("tmpfs", [])

            # Check for tmpfs usage for temporary data
            if service_name in ["web", "api"] and not tmpfs:
                storage_recommendations.append(
                    f"Service {service_name} could benefit from tmpfs for temporary files"
                )

            # Check volume mount options
            if isinstance(volumes, list):
                for volume in volumes:
                    if isinstance(volume, str) and ":" in volume:
                        host_path, container_path = volume.split(":", 1)

                        # Check for bind mounts vs named volumes
                        if host_path.startswith("/") or host_path.startswith("./"):
                            if service_name in ["web", "api"]:
                                storage_recommendations.append(
                                    f"Service {service_name} uses bind mount instead of named volume: {volume}"
                                )

        # Check volume definitions
        volumes_section = compose_config.get("volumes", {})

        for volume_name, volume_config in volumes_section.items():
            if isinstance(volume_config, dict):
                driver = volume_config.get("driver", "local")
                driver_opts = volume_config.get("driver_opts", {})

                if driver == "local" and not driver_opts:
                    storage_recommendations.append(
                        f"Volume {volume_name} uses default local driver - consider specific options"
                    )

        if storage_recommendations:
            print(
                "Storage performance recommendations:\n"
                + "\n".join(storage_recommendations)
            )

    def test_network_performance(self):
        """Test network performance configurations"""
        compose_file = Path(__file__).parent.parent.parent / "docker-compose.yml"

        if not compose_file.exists():
            pytest.skip("docker-compose.yml not found")

        try:
            with open(compose_file, "r") as f:
                compose_config = yaml.safe_load(f)
        except yaml.YAMLError as e:
            pytest.fail(f"Invalid docker-compose.yml: {e}")

        networks = compose_config.get("networks", {})
        services = compose_config.get("services", {})
        network_recommendations = []

        # Check for network driver optimizations
        for network_name, network_config in networks.items():
            if isinstance(network_config, dict):
                driver = network_config.get("driver", "bridge")

                if driver == "bridge":
                    # Check for bridge network optimizations
                    driver_opts = network_config.get("driver_opts", {})

                    if not driver_opts:
                        network_recommendations.append(
                            f"Network {network_name} could benefit from driver options"
                        )

                # Check for MTU configuration
                if "mtu" not in network_config:
                    network_recommendations.append(
                        f"Network {network_name} missing MTU configuration"
                    )

        # Check service network attachments
        multi_network_services = 0
        for service_name, service_config in services.items():
            service_networks = service_config.get("networks", [])

            if isinstance(service_networks, list) and len(service_networks) > 1:
                multi_network_services += 1

        if multi_network_services == 0:
            print("No services use multiple networks - all services on default network")

        if network_recommendations:
            print(
                "Network performance recommendations:\n"
                + "\n".join(network_recommendations)
            )
